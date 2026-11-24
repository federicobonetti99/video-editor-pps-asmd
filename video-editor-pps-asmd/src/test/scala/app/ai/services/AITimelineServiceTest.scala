package app.ai.services

import org.scalatest.funsuite.AnyFunSuite
import app.ai.capabilities.mock.{MockImageGeneratorCapability, MockIntentParserCapability}
import app.ai.capabilities.{AudioGeneratorCapability, ImageGeneratorCapability, IntentParserCapability}
import app.ai.domain.*
import core.engine.TimelineEngine
import core.model.*

import scala.util.boundary
import scala.util.boundary.break

class AITimelineServiceTest extends AnyFunSuite:

  given ImageGeneratorCapability = MockImageGeneratorCapability

  given AudioGeneratorCapability = new AudioGeneratorCapability:
    override def generateSpeech(text: String, voiceIdOpt: Option[String])(using boundary.Label[AIGenerationError]): GeneratedAudio =
      if text.trim.isEmpty then break(AIGenerationError("Testo vuoto."))
      else GeneratedAudio("/tmp/mock_speech.mp3", 3.5, text)

  private def stubParser(intentToReturn: UserIntent): IntentParserCapability =
    new IntentParserCapability:
      override def parse(text: String)(using boundary.Label[AIParseError]): UserIntent =
        if text.trim.isEmpty then break(AIParseError("Il comando inserito è vuoto."))
        else intentToReturn

  test("tool generate_image: genera immagine e la inserisce al currentTime"):
    given IntentParserCapability = MockIntentParserCapability
    val initialTimeline = Timeline.default.copy(currentTime = 3.0)

    val result = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("genera foto di un gatto", initialTimeline, defaultDuration = 4.0)

    result match
      case AIActionOutcome.UpdatedTimeline(updatedTimeline) =>
        val clips = updatedTimeline.videoTracks.head.clips
        assert(clips.length == 1)
        assert(clips.head.timing.startTime == 3.0)
        assert(clips.head.timing.duration == 4.0)
        assert(clips.head.sourceUrl.nonEmpty)
      case other => fail(s"Atteso UpdatedTimeline, ottenuto: $other")

  test("tool generate_speech: genera audio e lo inserisce al currentTime sulla traccia audio"):
    given IntentParserCapability = stubParser(UserIntent.GenerateSpeech("Ciao a tutti"))
    val initialTimeline = Timeline.default.copy(currentTime = 2.0)

    val result = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("genera voce che dice ciao", initialTimeline)

    result match
      case AIActionOutcome.UpdatedTimeline(updatedTimeline) =>
        val audioClips = updatedTimeline.audioTracks.head.clips
        assert(audioClips.length == 1)
        assert(audioClips.head.timing.startTime == 2.0)
        assert(audioClips.head.timing.duration == 3.5)
      case other => fail(s"Atteso UpdatedTimeline con clip audio, ottenuto: $other")

  test("tool cut_clip: taglia la clip al timestamp indicato"):
    given IntentParserCapability = stubParser(UserIntent.CutClip(4.0))
    val clip = ImageClip.create("file:///test.png", startTime = 0.0, duration = 10.0)
    val initialTimeline = TimelineEngine.addVideoClip(Timeline.default, 1, clip)

    val result = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("taglia al secondo 4", initialTimeline)

    result match
      case AIActionOutcome.UpdatedTimeline(updatedTimeline) =>
        val clips = updatedTimeline.videoTracks.head.clips
        assert(clips.length == 2)
        assert(clips(0).timing.duration == 4.0)
        assert(clips(1).timing.startTime == 4.0)
        assert(clips(1).timing.duration == 6.0)
      case other => fail(s"Atteso UpdatedTimeline con 2 clip tagliate, ottenuto: $other")

  test("tool apply_effect: applica effetto ZoomIn alla clip sotto il cursore"):
    given IntentParserCapability = stubParser(UserIntent.ApplyEffect("ZoomIn"))
    val clip = ImageClip.create("file:///test.png", startTime = 0.0, duration = 5.0)
    val initialTimeline = TimelineEngine.addVideoClip(Timeline.default, 1, clip).copy(currentTime = 2.0)

    val result = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("metti effetto zoom", initialTimeline)

    result match
      case AIActionOutcome.UpdatedTimeline(updatedTimeline) =>
        val updatedClip = updatedTimeline.videoTracks.head.clips.head
        assert(updatedClip.effect != VideoEffect.None)
      case other => fail(s"Atteso UpdatedTimeline con effetto applicato, ottenuto: $other")

  test("tool remove_silences: compatta tutte le clip rimuovendo gli spazi vuoti"):
    given IntentParserCapability = stubParser(UserIntent.RemoveSilences)
    val clip1 = ImageClip.create("file:///a.png", startTime = 0.0, duration = 3.0)
    val clip2 = ImageClip.create("file:///b.png", startTime = 7.0, duration = 4.0)
    val tl1 = TimelineEngine.addVideoClip(Timeline.default, 1, clip1)
    val initialTimeline = TimelineEngine.addVideoClip(tl1, 1, clip2)

    val result = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("togli i silenzi", initialTimeline)

    result match
      case AIActionOutcome.UpdatedTimeline(updatedTimeline) =>
        val clips = updatedTimeline.videoTracks.head.clips
        assert(clips.length == 2)
        assert(clips(0).timing.startTime == 0.0)
        assert(clips(1).timing.startTime == 3.0)
      case other => fail(s"Atteso UpdatedTimeline con clip compattate, ottenuto: $other")

  test("fast-path: 'annulla' restituisce TriggerUndo senza chiamare LLM"):
    given IntentParserCapability = MockIntentParserCapability
    val fastResult = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("annulla", Timeline.default)
    assert(fastResult == AIActionOutcome.TriggerUndo)

  test("tool undo: restituisce TriggerUndo via parser"):
    given IntentParserCapability = stubParser(UserIntent.Undo)
    val llmResult = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("torna indietro di una azione", Timeline.default)
    assert(llmResult == AIActionOutcome.TriggerUndo)

  test("propagate AIParseError on empty command"):
    given IntentParserCapability = MockIntentParserCapability
    val result = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("   ", Timeline.default)

    result match
      case err: AIParseError =>
        assert(err.reason.nonEmpty)
      case other => fail(s"Dovrebbe propagare AIParseError, ottenuto: $other")

  test("propagate AIGenerationError on unknown command"):
    given IntentParserCapability = MockIntentParserCapability
    val result = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("comando inesistente", Timeline.default)

    result match
      case err: AIGenerationError =>
        assert(err.reason.contains("Comando non riconosciuto"))
      case other => fail(s"Dovrebbe propagare AIGenerationError, ottenuto: $other")