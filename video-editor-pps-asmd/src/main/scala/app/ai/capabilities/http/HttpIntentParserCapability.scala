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
                                        accountIdOpt: Option[String] = sys.env.get("CF_ACCOUNT_ID").map(_.trim).filter(_.nonEmpty),
                                        apiTokenOpt: Option[String] = sys.env.get("CF_API_TOKEN").map(_.trim).filter(_.nonEmpty)
                                      ) extends IntentParserCapability:

  override def parse(text: String)(using Label[AIParseError]): UserIntent =
    val cleaned = text.trim
    if cleaned.isEmpty then break(AIParseError("Il comando inserito è vuoto."))

    val accountId = accountIdOpt.getOrElse(
      break(AIParseError("Variabile d'ambiente CF_ACCOUNT_ID mancante."))
    )
    val apiToken = apiTokenOpt.getOrElse(
      break(AIParseError("Variabile d'ambiente CF_API_TOKEN mancante."))
    )

    val systemPrompt =
      "You are an intent classifier for a video editor. Respond ONLY with a single raw JSON object, no markdown, no extra text. " +
        "Allowed values for 'intent': " +
        "'generate_image' (user asks to create/generate a single image; put a detailed English visual prompt in 'prompt'), " +
        "'remove_silences' (user asks to cut/remove silent parts in audio or video), " +
        "'cut_clip' (user asks to cut/split at a specific second; put seconds as number in 'timestamp'), " +
        "'apply_effect' (user asks to apply a visual effect like ZoomIn, Shake, FadeIn; put name in 'effect'), " +
        "'undo' (user asks to undo last action), " +
        "'unknown' (anything else). " +
        "Format: {\"intent\":\"...\",\"prompt\":\"...\",\"timestamp\":0.0,\"effect\":\"...\"}"

    val endpoint = s"https://api.cloudflare.com/client/v4/accounts/$accountId/ai/run/@cf/meta/llama-3.1-8b-instruct"
    val payload =
      s"""{
         |  "temperature": 0.1,
         |  "messages": [
         |    {"role": "system", "content": "${escapeJson(systemPrompt)}"},
         |    {"role": "user", "content": "${escapeJson(cleaned)}"}
         |  ]
         |}""".stripMargin

    val responseBody = Try {
      val request = HttpRequest.newBuilder()
        .uri(URI.create(endpoint))
        .timeout(Duration.ofSeconds(30))
        .header("Authorization", s"Bearer $apiToken")
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(payload, StandardCharsets.UTF_8))
        .build()

      val response = SharedHttpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
      if response.statusCode() == 200 then response.body()
      else break(AIParseError(s"Errore Cloudflare LLM (HTTP ${response.statusCode()}): ${response.body().take(250)}"))
    }.getOrElse(break(AIParseError("Errore di rete durante la chiamata a Cloudflare.")))

    parseCloudflareIntent(responseBody, cleaned)

  private[http] def parseCloudflareIntent(responseBody: String, rawText: String)(using Label[AIParseError]): UserIntent =
    val root = MiniJson.parse(responseBody) match
      case Right(v)  => v
      case Left(err) => break(AIParseError(s"JSON Cloudflare non valido ($err): ${responseBody.take(250)}"))

    val rObj = root.obj.getOrElse(
      break(AIParseError(s"Risposta Cloudflare non è un oggetto JSON: ${responseBody.take(250)}"))
    )

    val parsedObj: Map[String, JsonVal] = extractIntentObject(rObj) match
      case Some(obj) => obj
      case None =>
        val rawModelText = extractModelText(rObj).getOrElse(
          break(AIParseError(s"Formato risposta Cloudflare inatteso: ${responseBody.take(300)}"))
        )
        val jsonSlice = extractJsonObject(rawModelText)
        MiniJson.parse(jsonSlice).toOption.flatMap(_.obj).getOrElse(
          break(AIParseError(s"JSON intento non valido: $rawModelText"))
        )

    parsedObj.get("intent").flatMap(_.str).map(_.trim.toLowerCase) match
      case Some("generate_image") =>
        val prompt = parsedObj.get("prompt").flatMap(_.str).map(_.trim).filter(_.nonEmpty).getOrElse(rawText)
        UserIntent.GenerateImage(prompt)

      case Some("remove_silences") =>
        UserIntent.RemoveSilences

      case Some("cut_clip") =>
        parsedObj.get("timestamp").flatMap(_.double) match
          case Some(ts) if ts >= 0.0 => UserIntent.CutClip(ts)
          case _                     => break(AIParseError("Timestamp non valido per il taglio."))

      case Some("apply_effect") =>
        val effect = parsedObj.get("effect").flatMap(_.str).map(_.trim).filter(_.nonEmpty).getOrElse("ZoomIn")
        UserIntent.ApplyEffect(effect)

      case Some("undo") =>
        UserIntent.Undo

      case _ =>
        UserIntent.Unknown(rawText)

  private def extractIntentObject(rObj: Map[String, JsonVal]): Option[Map[String, JsonVal]] =
    for
      resObj    <- rObj.get("result").flatMap(_.obj)
      respAsObj <- resObj.get("response").flatMap(_.obj)
    yield respAsObj

  private def extractModelText(rObj: Map[String, JsonVal]): Option[String] =
    val fromResultResponse = for
      resObj <- rObj.get("result").flatMap(_.obj)
      resp   <- resObj.get("response").flatMap(_.str)
    yield resp

    val fromOpenAIChoices = for
      choices <- rObj.get("choices").flatMap(_.arr)
      first   <- choices.headOption.flatMap(_.obj)
      msg     <- first.get("message").flatMap(_.obj)
      content <- msg.get("content").flatMap(_.str)
    yield content

    val fromResultChoices = for
      resObj  <- rObj.get("result").flatMap(_.obj)
      choices <- resObj.get("choices").flatMap(_.arr)
      first   <- choices.headOption.flatMap(_.obj)
      msg     <- first.get("message").flatMap(_.obj)
      content <- msg.get("content").flatMap(_.str)
    yield content

    fromResultResponse.orElse(fromOpenAIChoices).orElse(fromResultChoices)

  private def extractJsonObject(text: String): String =
    val start = text.indexOf('{')
    val end = text.lastIndexOf('}')
    if start >= 0 && end > start then text.substring(start, end + 1)
    else text