package app.ai.capabilities.http

import app.ai.capabilities.ImageGeneratorCapability
import app.ai.domain.{AIGenerationError, GeneratedImage}

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.net.URI
import java.net.http.{HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.Duration
import java.util.{Base64, UUID}
import javax.imageio.ImageIO
import scala.util.Try
import scala.util.boundary.{Label, break}

final class HttpImageGeneratorCapability(
                                          accountIdOpt: Option[String] = sys.env.get("CF_ACCOUNT_ID").map(_.trim).filter(_.nonEmpty),
                                          apiTokenOpt: Option[String] = sys.env.get("CF_API_TOKEN").map(_.trim).filter(_.nonEmpty),
                                          outputDir: Path = Paths.get("ai_generated_assets")
                                        ) extends ImageGeneratorCapability:

  override def generateImage(prompt: String)(using Label[AIGenerationError]): GeneratedImage =
    val accountId = accountIdOpt.getOrElse(
      break(AIGenerationError("Variabile d'ambiente CF_ACCOUNT_ID mancante."))
    )
    val apiToken = apiTokenOpt.getOrElse(
      break(AIGenerationError("Variabile d'ambiente CF_API_TOKEN mancante."))
    )

    val endpoint = s"https://api.cloudflare.com/client/v4/accounts/$accountId/ai/run/@cf/black-forest-labs/flux-1-schnell"
    val payload = s"""{"prompt":"${escapeJson(prompt)}","steps":4}"""

    val responseBody = Try {
      val request = HttpRequest.newBuilder()
        .uri(URI.create(endpoint))
        .timeout(Duration.ofSeconds(60))
        .header("Authorization", s"Bearer $apiToken")
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(payload, StandardCharsets.UTF_8))
        .build()

      val response = SharedHttpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
      if response.statusCode() == 200 then response.body()
      else break(AIGenerationError(s"Errore Cloudflare (HTTP ${response.statusCode()}): ${response.body().take(250)}"))
    }.getOrElse(break(AIGenerationError("Errore di rete durante la chiamata a Cloudflare.")))

    val base64Opt = for
      root   <- MiniJson.parse(responseBody).toOption
      rObj   <- root.obj
      resObj <- rObj.get("result").flatMap(_.obj)
      imgStr <- resObj.get("image").flatMap(_.str)
    yield imgStr

    val b64 = base64Opt.getOrElse(
      break(AIGenerationError("Risposta Cloudflare priva del campo result.image."))
    )

    val bytes = Try(Base64.getDecoder.decode(b64)).getOrElse(
      break(AIGenerationError("Impossibile decodificare i byte Base64 dell'immagine."))
    )

    val rawImg = Try(Option(ImageIO.read(new ByteArrayInputStream(bytes)))).toOption.flatten.getOrElse(
      break(AIGenerationError("Formato immagine non leggibile da ImageIO."))
    )

    val croppedImg = cropTo16x9(rawImg)

    val savedPath = Try {
      Files.createDirectories(outputDir)
      val fileName = s"cf_flux_16x9_${System.currentTimeMillis()}_${UUID.randomUUID().toString.take(8)}.png"
      val target = outputDir.resolve(fileName)
      ImageIO.write(croppedImg, "png", target.toFile)
      target
    }.getOrElse(break(AIGenerationError("Impossibile salvare l'immagine ritagliata su disco.")))

    GeneratedImage(
      path = savedPath.toAbsolutePath.toUri.toString,
      width = croppedImg.getWidth,
      height = croppedImg.getHeight,
      prompt = prompt
    )

  private[http] def cropTo16x9(rawImg: BufferedImage): BufferedImage =
    val width = rawImg.getWidth
    val height = rawImg.getHeight
    val targetHeight = Math.min(height, (width * 9) / 16)
    val yOffset = (height - targetHeight) / 2
    if targetHeight < height then rawImg.getSubimage(0, yOffset, width, targetHeight)
    else rawImg