package app.ai.capabilities.http

import app.ai.domain.{AIGenerationError, AIParseError, UserIntent}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.awt.image.BufferedImage
import scala.util.boundary

class CloudflareCapabilitiesTest extends AnyFunSuite with Matchers:

  private val parserWithoutNetwork =
    new HttpIntentParserCapability(accountIdOpt = Some("acc"), apiTokenOpt = Some("tok"))

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

  test("HttpIntentParserCapability fallisce subito con AIParseError se credenziali o testo mancano") {
    val noCredsParser = new HttpIntentParserCapability(accountIdOpt = None, apiTokenOpt = None)

    val missingCreds = boundary[AIParseError | UserIntent]:
      noCredsParser.parse("genera un gatto")
    missingCreds shouldBe a[AIParseError]

    val blankInput = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parse("   ")
    blankInput shouldBe a[AIParseError]
  }

  test("HttpIntentParserCapability decodifica result.response sia come stringa JSON che come oggetto") {
    val asStringResponse =
      """{"result":{"response":"{\"intent\":\"generate_image\",\"prompt\":\"a red fox in snow\"}"}}"""
    val res1 = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parseCloudflareIntent(asStringResponse, "una volpe rossa")
    res1 shouldBe UserIntent.GenerateImage("a red fox in snow")

    val asObjectResponse =
      """{"result":{"response":{"intent":"remove_silences"}}}"""
    val res2 = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parseCloudflareIntent(asObjectResponse, "taglia i silenzi")
    res2 shouldBe UserIntent.RemoveSilences
  }

  test("HttpIntentParserCapability decodifica il formato OpenAI-compatible choices[0].message.content") {
    val openAiFormat =
      """{"choices":[{"message":{"content":"{\"intent\":\"cut_clip\",\"timestamp\":7.25}"}}]}"""
    val res = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parseCloudflareIntent(openAiFormat, "taglia a 7.25")
    res shouldBe UserIntent.CutClip(7.25)
  }

  test("HttpIntentParserCapability rifiuta timestamp negativi nel taglio clip") {
    val negativeTs =
      """{"result":{"response":{"intent":"cut_clip","timestamp":-3.0}}}"""
    val res = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parseCloudflareIntent(negativeTs, "taglia a -3")
    res shouldBe a[AIParseError]
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