package app.ai.capabilities

import org.scalatest.funsuite.AnyFunSuite
import app.ai.capabilities.mock.MockIntentParserCapability
import app.ai.domain.{AIParseError, UserIntent}
import scala.util.boundary

class IntentParserCapabilityTest extends AnyFunSuite:

  test("parse image generation intent correctly"):
    val result = boundary[AIParseError | UserIntent]:
      MockIntentParserCapability.parse("genera immagine di un drago")

    result match
      case UserIntent.GenerateImage(prompt) =>
        assert(prompt == "genera immagine di un drago")
      case _ => fail("Dovrebbe riconoscere l'intento di generare un'immagine")

  test("return AIParseError when command is empty"):
    val result = boundary[AIParseError | UserIntent]:
      MockIntentParserCapability.parse("   ")

    result match
      case err: AIParseError =>
        assert(err.reason == "Il comando non puo' essere vuoto")
      case _ => fail("Dovrebbe fallire per comando vuoto")

  test("return Unknown intent for unrecognized commands"):
    val result = boundary[AIParseError | UserIntent]:
      MockIntentParserCapability.parse("fai qualcosa di strano")

    result match
      case UserIntent.Unknown(text) =>
        assert(text == "fai qualcosa di strano")
      case _ => fail("Dovrebbe restituire un intento sconosciuto")