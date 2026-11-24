package app.ai

import app.ai.capabilities.http.HttpIntentParserCapability
import app.ai.capabilities.{AudioGeneratorCapability, ImageGeneratorCapability, IntentParserCapability}
import app.ai.domain.*
import app.ai.services.{AIActionOutcome, AITimelineService}
import core.engine.TimelineEngine
import core.model.*
import org.scalatest.Inside.inside
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.boundary

object IntegrationTest extends Tag("Integration")

class QuickBarIntegrationTest extends AnyFlatSpec with Matchers:

  given ImageGeneratorCapability with
    override def generateImage(prompt: String)(using boundary.Label[AIGenerationError]): GeneratedImage =
      GeneratedImage(path = "/tmp/stub_image.png", width = 1024, height = 576, prompt = prompt)

  given AudioGeneratorCapability with
    override def generateSpeech(text: String, voiceIdOpt: Option[String])(using boundary.Label[AIGenerationError]): GeneratedAudio =
      GeneratedAudio(path = "/tmp/stub_speech.mp3", durationSeconds = 3.0, prompt = text)

  private def withRealGroqParser(testCode: IntentParserCapability ?=> Unit): Unit =
    val keyOpt = sys.env.get("GROQ_API_KEY").map(_.trim).filter(_.nonEmpty)
    keyOpt match
      case Some(key) =>
        given IntentParserCapability = new HttpIntentParserCapability(groqKeyOpt = Some(key))
        testCode
      case None =>
        cancel("GROQ_API_KEY environment variable not set: real integration test skipped.")

  behavior of "QuickBar End-to-End with real Groq API"

  it should "add a real audio track when requested in natural language" taggedAs IntegrationTest in withRealGroqParser {
    val initial = Timeline.default
    val initialAudioCount = initial.audioTracks.length

    val result = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("add an audio track", initial)

    inside(result) {
      case AIActionOutcome.UpdatedTimeline(updated) =>
        updated.audioTracks should have length (initialAudioCount + 1)
    }
  }

  it should "add a real video track when requested in natural language" taggedAs IntegrationTest in withRealGroqParser {
    val initial = Timeline.default
    val initialVideoCount = initial.videoTracks.length

    val result = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("add a new video track", initial)

    inside(result) {
      case AIActionOutcome.UpdatedTimeline(updated) =>
        updated.videoTracks should have length (initialVideoCount + 1)
    }
  }

  it should "apply the Shake effect and then increase its intensity on a subsequent command" taggedAs IntegrationTest in withRealGroqParser {
    val clip = ImageClip.create("file:///sample.png", startTime = 0.0, duration = 6.0)
    val initial = TimelineEngine.addVideoClip(Timeline.default, 1, clip).copy(currentTime = 2.0)

    val step1 = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("apply a shake effect to the clip", initial)

    val tlWithShake = inside(step1) {
      case AIActionOutcome.UpdatedTimeline(tl) =>
        tl.videoTracks.head.clips.head.effect shouldBe a[VideoEffect.Shake]
        tl
    }

    val initialIntensity = tlWithShake.videoTracks.head.clips.head.effect match
      case VideoEffect.Shake(intensity, _) => intensity
      case other                           => fail(s"Unexpected effect: $other")

    val step2 = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("increase the shake intensity", tlWithShake)

    inside(step2) {
      case AIActionOutcome.UpdatedTimeline(tlUpdated) =>
        tlUpdated.videoTracks.head.clips.head.effect match
          case VideoEffect.Shake(newIntensity, _) =>
            newIntensity should be > initialIntensity
          case other =>
            fail(s"Effect was overwritten instead of adjusted: $other")
    }
  }

  it should "adjust the zoom parameter of a clip with an active ZoomIn effect" taggedAs IntegrationTest in withRealGroqParser {
    val clipWithZoom = ImageClip.create("file:///sample.png", startTime = 0.0, duration = 5.0)
      .withEffect(VideoEffect.ZoomIn(targetScale = 1.3))
    val initial = TimelineEngine.addVideoClip(Timeline.default, 1, clipWithZoom).copy(currentTime = 1.0)

    val result = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("increase the zoom even more", initial)

    inside(result) {
      case AIActionOutcome.UpdatedTimeline(updated) =>
        updated.videoTracks.head.clips.head.effect match
          case VideoEffect.ZoomIn(newScale) =>
            newScale should be > 1.3
          case other =>
            fail(s"Expected ZoomIn with targetScale > 1.3, got: $other")
    }
  }

  it should "set the audio volume to 35% on an existing audio clip" taggedAs IntegrationTest in withRealGroqParser {
    val audio = AudioClip(
      sourceUrl = "file:///voice.mp3",
      sourceLength = 8.0,
      timing = ClipTiming(startTime = 0.0, trimStart = 0.0, duration = 8.0),
      volume = 1.0
    )
    val initial = TimelineEngine.addAudioClip(Timeline.default, 1, audio).copy(currentTime = 2.0)

    val result = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("set the audio volume to 35%", initial)

    inside(result) {
      case AIActionOutcome.UpdatedTimeline(updated) =>
        updated.audioTracks.head.clips.head.volume shouldBe (0.35 +- 0.05)
    }
  }

  it should "cut the clip at the second specified in the command" taggedAs IntegrationTest in withRealGroqParser {
    val clip = ImageClip.create("file:///sample.png", startTime = 0.0, duration = 10.0)
    val initial = TimelineEngine.addVideoClip(Timeline.default, 1, clip)

    val result = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply("cut the clip at second 4", initial)

    inside(result) {
      case AIActionOutcome.UpdatedTimeline(updated) =>
        val clips = updated.videoTracks.head.clips
        clips should have length 2
        clips(0).timing.duration shouldBe (4.0 +- 0.01)
        clips(1).timing.startTime shouldBe (4.0 +- 0.01)
    }
  }