package app.ai.capabilities

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import app.ai.capabilities.mock.MockIntentParserCapability
import app.ai.domain.{AIParseError, UserIntent}
import scala.util.boundary

class IntentParserCapabilitySpec extends AnyFlatSpec:

  "MockIntentParserCapability" should "parse image generation intent correctly" in:
    val result = boundary[AIParseError | UserIntent]:
      MockIntentParserCapability.parse("genera immagine di un drago")

    result match
      case UserIntent.GenerateImage(prompt) =>
        prompt shouldBe "genera immagine di un drago"
      case _ => fail("Dovrebbe riconoscere l'intento di generare un'immagine")

  it should "return AIParseError when command is empty" in:
    val result = boundary[AIParseError | UserIntent]:
      MockIntentParserCapability.parse("   ")

    result match
      case err: AIParseError =>
        err.reason shouldBe "Il comando non puo' essere vuoto"
      case _ => fail("Dovrebbe fallire per comando vuoto")

  it should "return Unknown intent for unrecognized commands" in:
    val result = boundary[AIParseError | UserIntent]:
      MockIntentParserCapability.parse("fai qualcosa di strano")

    result match
      case UserIntent.Unknown(text) =>
        text shouldBe "fai qualcosa di strano"
      case _ => fail("Dovrebbe restituire un intento sconosciuto")