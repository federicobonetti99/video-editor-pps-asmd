package app.ai.capabilities.http

import app.ai.capabilities.ImageGeneratorCapability
import app.ai.domain.{AIGenerationError, GeneratedImage}
import java.net.{URI, URLEncoder}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.Duration
import scala.util.boundary.{Label, break}

object PollinationsImageGeneratorCapability extends ImageGeneratorCapability:

  private val Width = 1024
  private val Height = 576

  private val client = HttpClient.newBuilder()
    .connectTimeout(Duration.ofSeconds(15))
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

  override def generateImage(prompt: String)(using Label[AIGenerationError]): GeneratedImage =
    if prompt.trim.isEmpty then
      break(AIGenerationError("Il prompt non puo' essere vuoto"))

    val styledPrompt = s"${prompt.trim}, cinematic wide shot, high detail, photorealistic, dramatic lighting"
    val encodedPrompt = URLEncoder.encode(styledPrompt, StandardCharsets.UTF_8).replace("+", "%20")
    val url = s"https://image.pollinations.ai/prompt/$encodedPrompt?width=$Width&height=$Height&nologo=true"

    val tempFile =
      try Files.createTempFile("asmd_ai_", ".jpg")
      catch case e: Exception => break(AIGenerationError(s"Impossibile creare file temporaneo: ${e.getMessage}"))

    val request = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("User-Agent", "Mozilla/5.0")
      .timeout(Duration.ofSeconds(60))
      .GET()
      .build()

    val response =
      try client.send(request, HttpResponse.BodyHandlers.ofFile(tempFile))
      catch case e: Exception => break(AIGenerationError(s"Errore nel download dell'immagine: ${e.getMessage}"))

    if response.statusCode() != 200 then
      break(AIGenerationError(s"Errore HTTP ${response.statusCode()} dal servizio immagini"))

    GeneratedImage(
      path = tempFile.toAbsolutePath.toString,
      width = Width,
      height = Height,
      prompt = prompt.trim
    )