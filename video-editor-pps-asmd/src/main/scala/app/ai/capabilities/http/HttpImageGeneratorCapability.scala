package app.ai.capabilities.http

import app.ai.capabilities.ImageGeneratorCapability
import app.ai.domain.{AIGenerationError, GeneratedImage}
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.Files
import java.time.Duration
import scala.util.boundary.{Label, break}

class HttpImageGeneratorCapability(
                                    apiKey: String,
                                    baseUrl: String = "https://api.openai.com/v1",
                                    model: String = "dall-e-3"
                                  ) extends ImageGeneratorCapability:

  private val client = HttpClient.newBuilder()
    .connectTimeout(Duration.ofSeconds(15))
    .build()

  override def generateImage(prompt: String)(using Label[AIGenerationError]): GeneratedImage =
    if prompt.trim.isEmpty then
      break(AIGenerationError("Il prompt non puo' essere vuoto"))

    val body =
      s"""{"model":"${escapeJson(model)}","prompt":"${escapeJson(prompt.trim)}","n":1,"size":"1024x1024"}"""

    val request = HttpRequest.newBuilder()
      .uri(URI.create(s"$baseUrl/images/generations"))
      .header("Content-Type", "application/json")
      .header("Authorization", s"Bearer $apiKey")
      .timeout(Duration.ofSeconds(60))
      .POST(HttpRequest.BodyPublishers.ofString(body))
      .build()

    val response =
      try client.send(request, HttpResponse.BodyHandlers.ofString())
      catch case e: Exception => break(AIGenerationError(s"Errore di rete nella generazione: ${e.getMessage}"))

    if response.statusCode() != 200 then
      break(AIGenerationError(s"Errore HTTP ${response.statusCode()}: ${response.body()}"))

    val imageUrl = extractUrl(response.body()).getOrElse:
      break(AIGenerationError("URL immagine non trovato nella risposta"))

    val tempFile =
      try Files.createTempFile("asmd_ai_", ".png")
      catch case e: Exception => break(AIGenerationError(s"Impossibile creare file temporaneo: ${e.getMessage}"))

    val downloadRequest = HttpRequest.newBuilder()
      .uri(URI.create(imageUrl))
      .timeout(Duration.ofSeconds(30))
      .GET()
      .build()

    val downloadResponse =
      try client.send(downloadRequest, HttpResponse.BodyHandlers.ofFile(tempFile))
      catch case e: Exception => break(AIGenerationError(s"Errore nel download dell'immagine: ${e.getMessage}"))

    if downloadResponse.statusCode() != 200 then
      break(AIGenerationError(s"Download fallito con codice HTTP ${downloadResponse.statusCode()}"))

    GeneratedImage(
      path = tempFile.toAbsolutePath.toString,
      width = 1024,
      height = 1024,
      prompt = prompt.trim
    )

  private def extractUrl(json: String): Option[String] =
    val pattern = """"url"\s*:\s*"((?:[^"\\]|\\.)*)"""".r
    pattern.findFirstMatchIn(json).map(_.group(1).replace("\\u0026", "&"))

  private def escapeJson(s: String): String =
    s.replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")