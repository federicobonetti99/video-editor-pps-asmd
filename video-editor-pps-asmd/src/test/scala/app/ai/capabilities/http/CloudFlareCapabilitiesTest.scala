package app.ai.capabilities.http

import app.ai.domain.AIGenerationError
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.awt.image.BufferedImage
import scala.util.boundary

class CloudflareCapabilitiesTest extends AnyFunSuite with Matchers:

  test("MiniJson parsa correttamente oggetti annidati, array, numeri e caratteri escapati") {
    val raw = """{"success":true,"result":{"response":"{\"intent\":\"cut_clip\",\"timestamp\":12.5}"},"errors":[]}"""
    val parsed = MiniJson.parse(raw)

    parsed.isRight shouldBe true
    val successFlag = parsed.toOption.flatMap(_.obj).flatMap(_.get("success"))
    successFlag shouldBe Some(JsonVal.JBool(true))
  }

  test("MiniJson restituisce Left su JSON troncato o malformato") {
    MiniJson.parse("""{"result": {"response": "unterminated""").isLeft shouldBe true
  }

  test("HttpImageGeneratorCapability ritaglia un'immagine 1024x1024 esattamente in 1024x576 (16:9)") {
    val generator = new HttpImageGeneratorCapability(accountIdOpt = None, apiTokenOpt = None)
    val squareImg = new BufferedImage(1024, 1024, BufferedImage.TYPE_INT_RGB)

    val cropped = generator.cropTo16x9(squareImg)
    cropped.getWidth shouldBe 1024
    cropped.getHeight shouldBe 576
  }

  test("HttpImageGeneratorCapability fallisce subito con AIGenerationError se mancano le credenziali") {
    val generator = new HttpImageGeneratorCapability(accountIdOpt = None, apiTokenOpt = None)

    val res = boundary[AIGenerationError | Any]:
      generator.generateImage("an elephant")
    res shouldBe a[AIGenerationError]
  }