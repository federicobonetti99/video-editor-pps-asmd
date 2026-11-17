package app.ai.capabilities.http

import app.ai.domain.{AIParseError, UserIntent}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.util.boundary

class GroqCapabilitiesTest extends AnyFunSuite with Matchers:

  private val parserWithoutNetwork =
    new HttpIntentParserCapability(
      groqKeyOpt = Some("gsk_test_key"),
      preferredModelOpt = Some("llama-3.3-70b-versatile")
    )

  private def wrapGroqToolCall(argumentsJson: String): String =
    val escapedArgs = argumentsJson.replace("\\", "\\\\").replace("\"", "\\\"")
    s"""{"choices":[{"message":{"tool_calls":[{"function":{"name":"dispatch_editor_command","arguments":"$escapedArgs"}}]}}]}"""

  test("HttpIntentParserCapability fallisce subito con AIParseError se GROQ_API_KEY o testo mancano") {
    val noCredsParser = new HttpIntentParserCapability(groqKeyOpt = None)

    val missingCreds = boundary[AIParseError | UserIntent]:
      noCredsParser.parse("genera un gatto")
    missingCreds shouldBe a[AIParseError]

    val blankInput = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parse("   ")
    blankInput shouldBe a[AIParseError]
  }

  test("HttpIntentParserCapability decodifica correttamente tutti i tool_calls Groq") {
    val imgRes = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parseGroqToolIntent(
        wrapGroqToolCall("""{"action":"generate_image","prompt":"a red fox in snow"}"""),
        "una volpe rossa"
      )
    imgRes shouldBe UserIntent.GenerateImage("a red fox in snow")

    val cutRes = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parseGroqToolIntent(
        wrapGroqToolCall("""{"action":"cut_clip","timestamp":7.25}"""),
        "taglia a 7.25"
      )
    cutRes shouldBe UserIntent.CutClip(7.25)

    val effectRes = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parseGroqToolIntent(
        wrapGroqToolCall("""{"action":"apply_effect","effect":"ZoomIn"}"""),
        "metti zoom"
      )
    effectRes shouldBe UserIntent.ApplyEffect("ZoomIn")

    val silenceRes = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parseGroqToolIntent(
        wrapGroqToolCall("""{"action":"remove_silences"}"""),
        "togli i silenzi"
      )
    silenceRes shouldBe UserIntent.RemoveSilences

    val undoRes = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parseGroqToolIntent(
        wrapGroqToolCall("""{"action":"undo"}"""),
        "annulla"
      )
    undoRes shouldBe UserIntent.Undo

    val unknownRes = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parseGroqToolIntent(
        wrapGroqToolCall("""{"action":"unknown"}"""),
        "che ore sono"
      )
    unknownRes shouldBe UserIntent.Unknown("che ore sono")
  }

  test("HttpIntentParserCapability restituisce AIParseError se manca tool_calls nella risposta Groq") {
    val invalidGroqResp = """{"choices":[{"message":{"content":"ciao"}}]}"""
    val res = boundary[AIParseError | UserIntent]:
      parserWithoutNetwork.parseGroqToolIntent(invalidGroqResp, "test")
    res shouldBe a[AIParseError]
  }