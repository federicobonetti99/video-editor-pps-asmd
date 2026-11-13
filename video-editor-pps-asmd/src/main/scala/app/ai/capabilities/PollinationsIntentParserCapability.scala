package app.ai.capabilities.http

import app.ai.capabilities.IntentParserCapability
import app.ai.domain.{AIParseError, UserIntent}
import java.net.{URI, URLEncoder}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.time.Duration
import scala.util.boundary.{Label, break}

object PollinationsIntentParserCapability extends IntentParserCapability:

  private val client = HttpClient.newBuilder()
    .connectTimeout(Duration.ofSeconds(15))
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

  override def parse(command: String)(using Label[AIParseError]): UserIntent =
    val cleaned = command.trim
    if cleaned.isEmpty then
      break(AIParseError("Il comando non puo' essere vuoto"))

    val instruction =
      s"Sei un parser di comandi per un editor video. Se l'utente chiede di creare, generare o aggiungere un'immagine o foto, rispondi SOLO con: GENERATE_IMAGE|<descrizione dettagliata in inglese>. Altrimenti rispondi SOLO con: UNKNOWN|$cleaned. Comando: $cleaned"

    val encoded = URLEncoder.encode(instruction, StandardCharsets.UTF_8).replace("+", "%20")
    val request = HttpRequest.newBuilder()
      .uri(URI.create(s"https://text.pollinations.ai/$encoded"))
      .header("User-Agent", "Mozilla/5.0")
      .timeout(Duration.ofSeconds(30))
      .GET()
      .build()

    val response =
      try client.send(request, HttpResponse.BodyHandlers.ofString())
      catch case e: Exception => break(AIParseError(s"Errore di rete durante il parsing: ${e.getMessage}"))

    if response.statusCode() != 200 then
      break(AIParseError(s"Errore HTTP ${response.statusCode()} dal parser"))

    response.body().trim.split("\\|", 2) match
      case Array("GENERATE_IMAGE", prompt) if prompt.trim.nonEmpty =>
        UserIntent.GenerateImage(prompt.trim)
      case Array("UNKNOWN", raw) =>
        UserIntent.Unknown(raw.trim)
      case _ =>
        UserIntent.Unknown(cleaned)