package app.ai.capabilities.http

import app.ai.capabilities.IntentParserCapability
import app.ai.domain.{AIParseError, UserIntent}
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import scala.util.boundary.{Label, break}

class HttpIntentParserCapability(
                                  apiKey: String,
                                  baseUrl: String = "https://api.openai.com/v1",
                                  model: String = "gpt-4o-mini"
                                ) extends IntentParserCapability:

  private val client = HttpClient.newBuilder()
    .connectTimeout(Duration.ofSeconds(15))
    .build()

  private val systemPrompt =
    "Sei un parser di comandi per un editor video. " +
      "Analizza il comando dell'utente e rispondi SOLO in uno di questi due formati senza altro testo: " +
      "GENERATE_IMAGE|<descrizione dettagliata dell'immagine da generare> " +
      "oppure UNKNOWN|<comando originale>"

  override def parse(command: String)(using Label[AIParseError]): UserIntent =
    if command.trim.isEmpty then
      break(AIParseError("Il comando non puo' essere vuoto"))

    val body =
      s"""{"model":"${escapeJson(model)}","messages":[""" +
        s"""{"role":"system","content":"${escapeJson(systemPrompt)}"},""" +
        s"""{"role":"user","content":"${escapeJson(command.trim)}"}],"temperature":0.0}"""

    val request = HttpRequest.newBuilder()
      .uri(URI.create(s"$baseUrl/chat/completions"))
      .header("Content-Type", "application/json")
      .header("Authorization", s"Bearer $apiKey")
      .timeout(Duration.ofSeconds(30))
      .POST(HttpRequest.BodyPublishers.ofString(body))
      .build()

    val response =
      try client.send(request, HttpResponse.BodyHandlers.ofString())
      catch case e: Exception => break(AIParseError(s"Errore di rete durante il parsing: ${e.getMessage}"))

    if response.statusCode() != 200 then
      break(AIParseError(s"Errore HTTP ${response.statusCode()}: ${response.body()}"))

    val content = extractContent(response.body()).getOrElse:
      break(AIParseError("Risposta JSON non valida dal modello"))

    content.trim.split("\\|", 2) match
      case Array("GENERATE_IMAGE", prompt) if prompt.trim.nonEmpty =>
        UserIntent.GenerateImage(prompt.trim)
      case Array("UNKNOWN", raw) =>
        UserIntent.Unknown(raw.trim)
      case _ =>
        UserIntent.Unknown(command.trim)

  private def extractContent(json: String): Option[String] =
    val pattern = """"content"\s*:\s*"((?:[^"\\]|\\.)*)"""".r
    pattern.findFirstMatchIn(json).map(m => unescapeJson(m.group(1)))

  private def escapeJson(s: String): String =
    s.replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")

  private def unescapeJson(s: String): String =
    s.replace("\\\"", "\"")
      .replace("\\n", "\n")
      .replace("\\r", "\r")
      .replace("\\t", "\t")
      .replace("\\\\", "\\")