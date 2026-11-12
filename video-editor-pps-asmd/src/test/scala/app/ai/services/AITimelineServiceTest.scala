package app.ai.services

import org.scalatest.funsuite.AnyFunSuite
import app.ai.capabilities.mock.{MockImageGeneratorCapability, MockIntentParserCapability}
import app.ai.capabilities.{ImageGeneratorCapability, IntentParserCapability}
import app.ai.domain.*
import core.model.Timeline
import scala.util.boundary

class AITimelineServiceTest extends AnyFunSuite:

  given IntentParserCapability = MockIntentParserCapability
  given ImageGeneratorCapability = MockImageGeneratorCapability

  test("parse command, generate image and append clip at currentTime"):
    val initialTimeline = Timeline.default.copy(currentTime = 3.0)
    val result = boundary[AIGenerationError | AIParseError | Timeline]:
      AITimelineService.processAndApply("genera foto di un gatto", initialTimeline, defaultDuration = 4.0)

    result match
      case updatedTimeline: Timeline =>
        val clips = updatedTimeline.videoTracks.head.clips
        assert(clips.length == 1)
        assert(clips.head.timing.startTime == 3.0)
        assert(clips.head.timing.duration == 4.0)
        assert(clips.head.sourceUrl.nonEmpty)
      case _ => fail("Dovrebbe restituire una timeline aggiornata con la nuova clip")

  test("propagate AIParseError on empty command"):
    val result = boundary[AIGenerationError | AIParseError | Timeline]:
      AITimelineService.processAndApply("   ", Timeline.default)

    result match
      case err: AIParseError =>
        assert(err.reason.nonEmpty)
      case _ => fail("Dovrebbe propagare AIParseError")

  test("propagate AIGenerationError on unknown command"):
    val result = boundary[AIGenerationError | AIParseError | Timeline]:
      AITimelineService.processAndApply("comando inesistente", Timeline.default)

    result match
      case err: AIGenerationError =>
        assert(err.reason.contains("Comando non riconosciuto"))
      case _ => fail("Dovrebbe propagare AIGenerationError")