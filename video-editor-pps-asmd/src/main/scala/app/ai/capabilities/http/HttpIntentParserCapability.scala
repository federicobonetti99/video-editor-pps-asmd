package app.ai.capabilities.http

import app.ai.capabilities.IntentParserCapability
import app.ai.domain.{AIParseError, UserIntent}

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

  override def parse(text: String)(using Label[AIParseError]): UserIntent =
    val cleaned = text.trim
    if cleaned.isEmpty then break(AIParseError("Il comando inserito è vuoto."))

    val groqKey = groqKeyOpt.getOrElse(
      break(AIParseError("Variabile d'ambiente GROQ_API_KEY mancante."))
    )

    val modelId = preferredModelOpt.getOrElse(resolveBestGroqModel(groqKey))

    val systemInstruction =
      "Sei l'interprete e router dei comandi della barra rapida di un video editor. " +
        "Analizza la richiesta dell'utente (in qualsiasi lingua) e invoca SEMPRE il tool dispatch_editor_command scegliendo l'azione esatta:\n" +
        "- 'generate_image': l'utente chiede di creare/generare un'immagine. Traduci e arricchisci la descrizione in un prompt visivo dettagliato in inglese dentro 'prompt'.\n" +
        "- 'cut_clip': l'utente chiede di tagliare/dividere una clip a un determinato secondo (es. 'taglia a 5 secondi', 'cut at 3.5'). Inserisci i secondi in 'timestamp'. Se dice 'qui' o non specifica un numero, imposta 'timestamp' a -1.0.\n" +
        "- 'apply_effect': l'utente chiede di applicare un effetto visivo. Normalizza 'effect' in uno di questi valori esatti: 'ZoomIn', 'Shake', 'FadeIn', 'None'.\n" +
        "- 'remove_silences': l'utente chiede di rimuovere i silenzi o le pause vuote dalle tracce (es. 'togli i silenzi', 'compatta').\n" +
        "- 'undo': l'utente chiede di annullare l'ultima azione (es. 'annulla', 'torna indietro', 'undo').\n" +
        "- 'unknown': qualsiasi altra richiesta non supportata."

    val payload =
      s"""{
         |  "model": "${escapeJson(modelId)}",
         |  "temperature": 0.1,
         |  "tool_choice": "required",
         |  "messages": [
         |    {"role": "system", "content": "${escapeJson(systemInstruction)}"},
         |    {"role": "user", "content": "${escapeJson(cleaned)}"}
         |  ],
         |  "tools": [
         |    {
         |      "type": "function",
         |      "function": {
         |        "name": "dispatch_editor_command",
         |        "description": "Decide ed esegue il singolo comando richiesto dall'utente nella barra rapida dell'editor video.",
         |        "parameters": {
         |          "type": "object",
         |          "properties": {
         |            "action": {
         |              "type": "string",
         |              "enum": ["generate_image", "cut_clip", "apply_effect", "remove_silences", "undo", "unknown"],
         |              "description": "L'azione da eseguire sulla timeline."
         |            },
         |            "prompt": {
         |              "type": "string",
         |              "description": "Prompt visivo dettagliato in inglese (solo per generate_image)."
         |            },
         |            "timestamp": {
         |              "type": "number",
         |              "description": "Secondo in cui effettuare il taglio, oppure -1.0 se l'utente intende la posizione attuale del cursore (solo per cut_clip)."
         |            },
         |            "effect": {
         |              "type": "string",
         |              "enum": ["ZoomIn", "Shake", "FadeIn", "None"],
         |              "description": "Nome dell'effetto video da applicare (solo per apply_effect)."
         |            }
         |          },
         |          "required": ["action"]
         |        }
         |      }
         |    }
         |  ]
         |}""".stripMargin

    val responseBody = Try {
      val request = HttpRequest.newBuilder()
        .uri(URI.create("https://api.groq.com/openai/v1/chat/completions"))
        .timeout(Duration.ofSeconds(30))
        .header("Authorization", s"Bearer $groqKey")
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(payload, StandardCharsets.UTF_8))
        .build()

      val response = SharedHttpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
      if response.statusCode() == 200 then response.body()
      else break(AIParseError(s"Errore Groq (HTTP ${response.statusCode()}): ${response.body().take(250)}"))
    }.getOrElse(break(AIParseError("Errore di rete durante la chiamata a Groq.")))

    parseGroqToolIntent(responseBody, cleaned)

  private def resolveBestGroqModel(groqKey: String)(using Label[AIParseError]): String =
    val body = Try {
      val request = HttpRequest.newBuilder()
        .uri(URI.create("https://api.groq.com/openai/v1/models"))
        .timeout(Duration.ofSeconds(20))
        .header("Authorization", s"Bearer $groqKey")
        .GET()
        .build()

      val response = SharedHttpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
      if response.statusCode() == 200 then response.body()
      else break(AIParseError(s"Impossibile leggere i modelli Groq (HTTP ${response.statusCode()})."))
    }.getOrElse(break(AIParseError("Errore di rete nel recupero dei modelli Groq.")))

    val root = MiniJson.parse(body).getOrElse(
      break(AIParseError("Risposta JSON non valida dall'endpoint modelli Groq."))
    )

    val ids = for
      obj    <- root.obj.toList
      data   <- obj.get("data").flatMap(_.arr).toList
      item   <- data
      fields <- item.obj.toList
      id     <- fields.get("id").flatMap(_.str).toList
      if GroqStoryboardPlannerCapability.scoreGroqModel(id) > 0
    yield id

    ids.distinct
      .sortBy(id => -GroqStoryboardPlannerCapability.scoreGroqModel(id))
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
      argsStr <- fnObj.get("arguments").flatMap(_.str).toList
    yield argsStr

    val argsJsonStr = toolCalls.headOption.getOrElse(
      break(AIParseError(s"Nessuna tool_call trovata nella risposta Groq: ${responseBody.take(300)}"))
    )

    val argsObj = MiniJson.parse(argsJsonStr).toOption.flatMap(_.obj).getOrElse(
      break(AIParseError(s"Argomenti della tool_call non validi: $argsJsonStr"))
    )

    argsObj.get("action").flatMap(_.str).map(_.trim.toLowerCase) match
      case Some("generate_image") =>
        val prompt = argsObj.get("prompt").flatMap(_.str).map(_.trim).filter(_.nonEmpty).getOrElse(rawText)
        UserIntent.GenerateImage(prompt)

      case Some("cut_clip") =>
        val ts = argsObj.get("timestamp").flatMap(_.double).getOrElse(-1.0)
        UserIntent.CutClip(ts)

      case Some("apply_effect") =>
        val effect = argsObj.get("effect").flatMap(_.str).map(_.trim).filter(_.nonEmpty).getOrElse("ZoomIn")
        UserIntent.ApplyEffect(effect)

      case Some("remove_silences") =>
        UserIntent.RemoveSilences

      case Some("undo") =>
        UserIntent.Undo

      case _ =>
        UserIntent.Unknown(rawText)