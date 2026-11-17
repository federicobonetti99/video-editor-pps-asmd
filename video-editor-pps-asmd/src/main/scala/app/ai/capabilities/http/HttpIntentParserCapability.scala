package app.ai.capabilities.http

import app.ai.capabilities.IntentParserCapability
import app.ai.domain.{AIParseError, AdjustDirection, TrackType, UserIntent}

import java.net.URI
import java.net.http.{HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.time.Duration
import scala.util.Try
import scala.util.boundary.{Label, break}

final class HttpIntentParserCapability(
                                        groqKeyOpt: Option[String] = sys.env.get("GROQ_API_KEY").map(_.trim).filter(_.nonEmpty),
                                        preferredModelOpt: Option[String] = None
                                      ) extends IntentParserCapability:

  @volatile private var cachedModelId: Option[String] = preferredModelOpt

  override def parse(text: String)(using Label[AIParseError]): UserIntent =
    val cleaned = text.trim
    if cleaned.isEmpty then break(AIParseError("Il comando inserito è vuoto."))

    val groqKey = groqKeyOpt.getOrElse(
      break(AIParseError("Variabile d'ambiente GROQ_API_KEY mancante."))
    )

    val modelId = cachedModelId.getOrElse {
      val resolved = resolveBestToolModel(groqKey)
      cachedModelId = Some(resolved)
      resolved
    }

    val systemInstruction =
      "Sei l'interprete e router dei comandi della barra rapida di un video editor. " +
        "Analizza la richiesta dell'utente e invoca SEMPRE uno dei tool disponibili:\n" +
        "- generate_image: creare/generare un'immagine (traduci 'prompt' in inglese).\n" +
        "- cut_clip: tagliare/dividere una clip al secondo 'timestamp' (-1.0 se al cursore).\n" +
        "- apply_effect: applicare un nuovo effetto ('ZoomIn', 'Shake', 'FadeIn', 'Brightness', 'Grayscale', 'Sepia', 'Invert', 'None').\n" +
        "- adjust_effect: modificare i parametri di un effetto già attivo ('increase', 'decrease', 'set').\n" +
        "- adjust_audio_volume: modificare il volume dell'audio ('increase', 'decrease', 'set', con 'value' lineare 0.0-2.0).\n" +
        "- add_track: aggiungere una nuova traccia 'video' o 'audio'.\n" +
        "- remove_silences: rimuovere i silenzi o compattare le tracce.\n" +
        "- undo: annullare l'ultima azione.\n" +
        "- unknown: qualsiasi altra richiesta non supportata."

    val toolsJson =
      """[
        |  {"type":"function","function":{"name":"generate_image","description":"Genera una nuova immagine.","parameters":{"type":"object","properties":{"prompt":{"type":"string"}},"required":["prompt"]}}},
        |  {"type":"function","function":{"name":"cut_clip","description":"Taglia la clip al timestamp.","parameters":{"type":"object","properties":{"timestamp":{"type":"number"}},"required":["timestamp"]}}},
        |  {"type":"function","function":{"name":"apply_effect","description":"Applica un effetto visivo.","parameters":{"type":"object","properties":{"effect":{"type":"string","enum":["ZoomIn","Shake","FadeIn","Brightness","Grayscale","Sepia","Invert","None"]}},"required":["effect"]}}},
        |  {"type":"function","function":{"name":"adjust_effect","description":"Modifica i parametri dell'effetto attivo.","parameters":{"type":"object","properties":{"direction":{"type":"string","enum":["increase","decrease","set"]},"parameter":{"type":"string","enum":["intensity","frequency","scale","duration","brightness"]},"amount":{"type":"number"}},"required":["direction"]}}},
        |  {"type":"function","function":{"name":"adjust_audio_volume","description":"Modifica il volume audio.","parameters":{"type":"object","properties":{"direction":{"type":"string","enum":["increase","decrease","set"]},"value":{"type":"number"}},"required":["direction"]}}},
        |  {"type":"function","function":{"name":"add_track","description":"Aggiunge una traccia video o audio.","parameters":{"type":"object","properties":{"track_type":{"type":"string","enum":["video","audio"]}},"required":["track_type"]}}},
        |  {"type":"function","function":{"name":"remove_silences","description":"Compatta le clip rimuovendo i silenzi.","parameters":{"type":"object","properties":{}}}},
        |  {"type":"function","function":{"name":"undo","description":"Annulla l'ultima azione.","parameters":{"type":"object","properties":{}}}},
        |  {"type":"function","function":{"name":"unknown","description":"Comando non riconosciuto.","parameters":{"type":"object","properties":{}}}}
        |]""".stripMargin

    val payload =
      s"""{
         |  "model": "${escapeJson(modelId)}",
         |  "temperature": 0.0,
         |  "tool_choice": "required",
         |  "messages": [
         |    {"role": "system", "content": "${escapeJson(systemInstruction)}"},
         |    {"role": "user", "content": "${escapeJson(cleaned)}"}
         |  ],
         |  "tools": $toolsJson
         |}""".stripMargin

    val responseBody = sendGroqCompletionWithRetry(groqKey, payload, retriesLeft = 3)
    parseGroqToolIntent(responseBody, cleaned)

  private def extractRetryDelayMs(resp: HttpResponse[String]): Long =
    val headerMs = Option(resp.headers().firstValue("retry-after").orElse(null))
      .flatMap(_.trim.toDoubleOption)
      .map(s => (s * 1000.0).toLong + 250L)

    val bodyRegex = """try again in ([0-9]+(?:\.[0-9]+)?)s""".r
    val bodyMs = bodyRegex.findFirstMatchIn(resp.body())
      .flatMap(_.group(1).toDoubleOption)
      .map(s => (s * 1000.0).toLong + 250L)

    headerMs.orElse(bodyMs).getOrElse(2000L).min(15000L).max(500L)

  private def sendGroqCompletionWithRetry(groqKey: String, payload: String, retriesLeft: Int)(using Label[AIParseError]): String =
    val request = HttpRequest.newBuilder()
      .uri(URI.create("https://api.groq.com/openai/v1/chat/completions"))
      .timeout(Duration.ofSeconds(30))
      .header("Authorization", s"Bearer $groqKey")
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(payload, StandardCharsets.UTF_8))
      .build()

    Try(SharedHttpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))) match
      case scala.util.Success(resp) if resp.statusCode() == 200 =>
        resp.body()
      case scala.util.Success(resp) if retriesLeft > 0 && resp.statusCode() == 429 =>
        Thread.sleep(extractRetryDelayMs(resp))
        sendGroqCompletionWithRetry(groqKey, payload, retriesLeft - 1)
      case scala.util.Success(resp) if retriesLeft > 0 && (resp.statusCode() == 400 || resp.statusCode() >= 500) =>
        Thread.sleep(400L)
        sendGroqCompletionWithRetry(groqKey, payload, retriesLeft - 1)
      case scala.util.Success(resp) =>
        break(AIParseError(s"Errore Groq (HTTP ${resp.statusCode()}): ${resp.body().take(250)}"))
      case scala.util.Failure(_) if retriesLeft > 0 =>
        Thread.sleep(400L)
        sendGroqCompletionWithRetry(groqKey, payload, retriesLeft - 1)
      case scala.util.Failure(err) =>
        break(AIParseError(s"Errore di rete durante la chiamata a Groq (${err.getClass.getSimpleName}: ${Option(err.getMessage).getOrElse("nessun dettaglio")})."))

  private def scoreToolCallingModel(id: String): Int =
    val lower = id.toLowerCase
    if lower.contains("whisper") || lower.contains("guard") || lower.contains("tts") || lower.contains("playai") then 0
    else if lower.contains("llama-3.3-70b") then 100
    else if lower.contains("llama-3.1-8b") then 90
    else if lower.contains("llama-3.1-70b") then 85
    else if lower.contains("llama3-70b") then 75
    else if lower.contains("mixtral") then 60
    else if lower.contains("gemma2") then 55
    else GroqStoryboardPlannerCapability.scoreGroqModel(id)

  private def resolveBestToolModel(groqKey: String)(using Label[AIParseError]): String =
    val request = HttpRequest.newBuilder()
      .uri(URI.create("https://api.groq.com/openai/v1/models"))
      .timeout(Duration.ofSeconds(20))
      .header("Authorization", s"Bearer $groqKey")
      .GET()
      .build()

    val response = Try(SharedHttpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8)))
      .orElse {
        Thread.sleep(300L)
        Try(SharedHttpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8)))
      }
      .fold(
        err => break(AIParseError(s"Errore di rete nel recupero dei modelli Groq (${err.getClass.getSimpleName}: ${Option(err.getMessage).getOrElse("nessun dettaglio")}).")),
        identity
      )

    val body =
      if response.statusCode() == 200 then response.body()
      else break(AIParseError(s"Impossibile leggere i modelli Groq (HTTP ${response.statusCode()})."))

    val root = MiniJson.parse(body).getOrElse(
      break(AIParseError("Risposta JSON non valida dall'endpoint modelli Groq."))
    )

    val ids = for
      obj    <- root.obj.toList
      data   <- obj.get("data").flatMap(_.arr).toList
      item   <- data
      fields <- item.obj.toList
      id     <- fields.get("id").flatMap(_.str).toList
      if scoreToolCallingModel(id) > 0
    yield id

    ids.distinct
      .sortBy(id => -scoreToolCallingModel(id))
      .headOption
      .getOrElse(break(AIParseError("Nessun modello chat disponibile sul tuo account Groq.")))

  private[http] def parseGroqToolIntent(responseBody: String, rawText: String)(using Label[AIParseError]): UserIntent =
    val root = MiniJson.parse(responseBody) match
      case Right(v)  => v
      case Left(err) => break(AIParseError(s"JSON Groq non valido ($err): ${responseBody.take(250)}"))

    val toolCalls = for
      rootObj <- root.obj.toList
      choices <- rootObj.get("choices").flatMap(_.arr).toList
      first   <- choices.headOption.flatMap(_.obj).toList
      msg     <- first.get("message").flatMap(_.obj).toList
      calls   <- msg.get("tool_calls").flatMap(_.arr).toList
      call    <- calls
      callObj <- call.obj.toList
      fnObj   <- callObj.get("function").flatMap(_.obj).toList
      fnName  <- fnObj.get("name").flatMap(_.str).toList
      argsStr <- fnObj.get("arguments").flatMap(_.str).toList
    yield (fnName.trim.toLowerCase, argsStr)

    val (fnName, argsJsonStr) = toolCalls.headOption.getOrElse(
      break(AIParseError(s"Nessuna tool_call trovata nella risposta Groq: ${responseBody.take(300)}"))
    )

    val argsObj = MiniJson.parse(argsJsonStr).toOption.flatMap(_.obj).getOrElse(Map.empty)

    def parseDirection: AdjustDirection =
      argsObj.get("direction").flatMap(_.str).map(_.trim.toLowerCase) match
        case Some("decrease") => AdjustDirection.Decrease
        case Some("set")      => AdjustDirection.Set
        case _                => AdjustDirection.Increase

    val actionName =
      if fnName == "dispatch_editor_command" then
        argsObj.get("action").flatMap(_.str).map(_.trim.toLowerCase).getOrElse("unknown")
      else fnName

    actionName match
      case "generate_image" =>
        val prompt = argsObj.get("prompt").flatMap(_.str).map(_.trim).filter(_.nonEmpty).getOrElse(rawText)
        UserIntent.GenerateImage(prompt)

      case "cut_clip" =>
        val ts = argsObj.get("timestamp").flatMap(_.double).getOrElse(-1.0)
        UserIntent.CutClip(ts)

      case "apply_effect" =>
        val effect = argsObj.get("effect").flatMap(_.str).map(_.trim).filter(_.nonEmpty).getOrElse("ZoomIn")
        UserIntent.ApplyEffect(effect)

      case "adjust_effect" =>
        val param = argsObj.get("parameter").flatMap(_.str).map(_.trim.toLowerCase).filter(_.nonEmpty)
        val amount = argsObj.get("amount").flatMap(_.double)
        UserIntent.AdjustEffect(parseDirection, param, amount)

      case "adjust_audio_volume" =>
        val value = argsObj.get("value").flatMap(_.double)
        UserIntent.AdjustAudioVolume(parseDirection, value)

      case "add_track" =>
        val tType = argsObj.get("track_type").flatMap(_.str).map(_.trim.toLowerCase) match
          case Some("audio") => TrackType.Audio
          case _             => TrackType.Video
        UserIntent.AddTrack(tType)

      case "remove_silences" =>
        UserIntent.RemoveSilences

      case "undo" =>
        UserIntent.Undo

      case _ =>
        UserIntent.Unknown(rawText)