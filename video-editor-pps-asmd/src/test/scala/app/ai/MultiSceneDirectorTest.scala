package app.ai

import app.ai.agents.MultiSceneDirectorWorkflow
import app.ai.capabilities.{AudioGeneratorCapability, ImageGeneratorCapability, StoryboardPlannerCapability}
import app.ai.capabilities.http.GroqStoryboardPlannerCapability
import app.ai.domain.*
import app.ai.services.AIDirectorService
import core.model.*
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.util.boundary
import scala.util.boundary.{Label, break}

final class MultiSceneDirectorTest extends AnyFunSuite:

  given AudioGeneratorCapability with
    override def generateSpeech(text: String, voiceIdOpt: Option[String])(using Label[AIGenerationError]): GeneratedAudio =
      GeneratedAudio(path = "/tmp/mock_narration.mp3", durationSeconds = 2.0, prompt = text)

  test("GroqStoryboardPlannerCapability selects the best chat model while filtering out non-chat models"):
    val candidates = List(
      "whisper-large-v3",
      "llama-guard-3-8b",
      "llama-3.1-8b-instant",
      "meta-llama/llama-4-maverick-17b-128e-instruct",
      "llama-3.3-70b-versatile"
    )
    val best = candidates
      .filter(id => GroqStoryboardPlannerCapability.scoreGroqModel(id) > 0)
      .sortBy(id => -GroqStoryboardPlannerCapability.scoreGroqModel(id))
      .headOption

    assert(best.contains("meta-llama/llama-4-maverick-17b-128e-instruct"))

  test("GroqStoryboardPlannerCapability parses and sorts scenes from tool_call JSON"):
    val planner = new GroqStoryboardPlannerCapability(groqKeyOpt = Some("dummy-key"))
    val mockGroqJson =
      """{
        |  "choices": [{
        |    "message": {
        |      "tool_calls": [{
        |        "function": {
        |          "name": "dispatch_visual_artists",
        |          "arguments": "{\"visual_style_bible\":\"Pixar 3D style, orange cat astronaut\",\"scenes\":[{\"scene_number\":2,\"shot_type\":\"DynamicMedium\",\"narration\":\"Meets the robot mouse.\",\"visual_prompt\":\"Meets a friendly robot mouse\"},{\"scene_number\":1,\"shot_type\":\"EstablishingWide\",\"narration\":\"Lands on the planet.\",\"visual_prompt\":\"Lands on a cheese planet\"}]}"
        |        }
        |      }]
        |    }
        |  }]
        |}""".stripMargin

    val result = boundary[AIDirectorError | StoryboardPlan]:
      planner.parseStoryboardFromGroq(mockGroqJson)

    result match
      case plan: StoryboardPlan =>
        assert(plan.styleBible == "Pixar 3D style, orange cat astronaut")
        assert(plan.scenes.map(_.sceneNumber) == List(1, 2))
        assert(plan.scenes.head.shotType == "EstablishingWide")
      case err: AIDirectorError =>
        fail(s"Valid JSON parsing failed: ${err.reason}")

  test("GroqStoryboardPlannerCapability returns AIDirectorError when API credentials are missing"):
    val planner = new GroqStoryboardPlannerCapability(groqKeyOpt = None)
    val result = boundary[AIDirectorError | StoryboardPlan]:
      planner.createStoryboard("A sample story")

    assert(result.isInstanceOf[AIDirectorError])

  test("MultiSceneDirectorWorkflow enriches prompts with styleBible and populates AgenticScope in order"):
    val capturedPrompts = new CopyOnWriteArrayList[String]()

    given StoryboardPlannerCapability with
      override def createStoryboard(story: String)(using Label[AIDirectorError]): StoryboardPlan =
        StoryboardPlan(
          styleBible = "Watercolor storybook style",
          scenes = List(
            SceneDirective(2, "CloseUp", "Scene 2", "Watercolor storybook style, smiling fox", 4.0),
            SceneDirective(1, "WideShot", "Scene 1", "A forest at dawn", 6.0)
          )
        )

    given ImageGeneratorCapability with
      override def generateImage(prompt: String)(using Label[AIGenerationError]): GeneratedImage =
        capturedPrompts.add(prompt)
        val tag = if prompt.contains("dawn") then "scene1.png" else "scene2.png"
        GeneratedImage(path = s"/tmp/$tag", width = 1024, height = 576, prompt = prompt)

    val outcome = Await.result(MultiSceneDirectorWorkflow.produceScenes("Story of the fox"), 5.seconds)

    outcome match
      case Left(err) => fail(s"Workflow failed unexpectedly: $err")
      case Right(scope) =>
        val prompts = capturedPrompts.asScala.toList
        assert(prompts.exists(_ == "Watercolor storybook style. A forest at dawn"))
        assert(prompts.exists(_ == "Watercolor storybook style, smiling fox"))

        val assets = scope.orderedAssets
        assert(assets.map(_.directive.sceneNumber) == List(1, 2))
        assert(assets.map(_.image.path) == List("/tmp/scene1.png", "/tmp/scene2.png"))
        assert(assets.forall(_.audioPathOpt.contains("/tmp/mock_narration.mp3")))
        assert(scope.executionTrace.size == 2)

  test("MultiSceneDirectorWorkflow propagates AIGenerationError when a scene generation fails"):
    given StoryboardPlannerCapability with
      override def createStoryboard(story: String)(using Label[AIDirectorError]): StoryboardPlan =
        StoryboardPlan(
          styleBible = "Cyberpunk neon",
          scenes = List(
            SceneDirective(1, "Wide", "Ok", "Valid scene"),
            SceneDirective(2, "CloseUp", "Boom", "FAIL_SCENE")
          )
        )

    given ImageGeneratorCapability with
      override def generateImage(prompt: String)(using Label[AIGenerationError]): GeneratedImage =
        if prompt.contains("FAIL_SCENE") then break(AIGenerationError("Cloudflare quota exceeded"))
        else GeneratedImage("/tmp/ok.png", 1024, 576, prompt)

    val outcome = Await.result(MultiSceneDirectorWorkflow.produceScenes("Error test"), 5.seconds)
    assert(outcome == Left(AIGenerationError("Cloudflare quota exceeded")))

  test("AIDirectorService assembles scenes and synchronized audio starting at the Timeline cursor"):
    given StoryboardPlannerCapability with
      override def createStoryboard(story: String)(using Label[AIDirectorError]): StoryboardPlan =
        StoryboardPlan(
          styleBible = "Anime 90s",
          scenes = List(
            SceneDirective(1, "EstablishingWide", "Start", "City skyline", 3.5),
            SceneDirective(2, "ActionMedium", "Action", "Hero running", 4.5)
          )
        )

    given ImageGeneratorCapability with
      override def generateImage(prompt: String)(using Label[AIGenerationError]): GeneratedImage =
        val file = if prompt.contains("skyline") then "s1.png" else "s2.png"
        GeneratedImage(s"/tmp/$file", 1024, 576, prompt)

    val initialTimeline = Timeline.default.copy(currentTime = 10.0)
    val result = Await.result(AIDirectorService.generateAndAppendStory("Anime test", initialTimeline), 5.seconds)

    result match
      case Left(err) => fail(s"Assembly failed: $err")
      case Right(directed) =>
        val videoClips = directed.timeline.videoTracks.head.clips
        assert(videoClips.size == 2)
        assert(videoClips(0).sourceUrl.endsWith("s1.png"))
        assert(videoClips(0).startTime == 10.0)
        assert(videoClips(0).duration == 3.5)
        assert(videoClips(1).sourceUrl.endsWith("s2.png"))
        assert(videoClips(1).startTime == 13.5)
        assert(videoClips(1).duration == 4.5)

        val audioClips = directed.timeline.audioTracks.head.clips
        assert(audioClips.size == 2)
        assert(audioClips(0).sourceUrl.endsWith("mock_narration.mp3"))
        assert(audioClips(0).startTime == 10.0)
        assert(audioClips(1).startTime == 13.5)

  test("AIDirectorService extends video scene duration when narration exceeds base duration"):
    given StoryboardPlannerCapability with
      override def createStoryboard(story: String)(using Label[AIDirectorError]): StoryboardPlan =
        StoryboardPlan(
          styleBible = "Sci-fi",
          scenes = List(
            SceneDirective(1, "Wide", "A very long narration sentence", "Spaceship orbit", 4.0)
          )
        )

    given ImageGeneratorCapability with
      override def generateImage(prompt: String)(using Label[AIGenerationError]): GeneratedImage =
        GeneratedImage("/tmp/ship.png", 1024, 576, prompt)

    given AudioGeneratorCapability with
      override def generateSpeech(text: String, voiceIdOpt: Option[String])(using Label[AIGenerationError]): GeneratedAudio =
        GeneratedAudio("/tmp/long_voice.mp3", durationSeconds = 6.2, prompt = text)

    val result = Await.result(AIDirectorService.generateAndAppendStory("Sci-fi test", Timeline.default), 5.seconds)

    result match
      case Left(err) => fail(s"Assembly failed: $err")
      case Right(directed) =>
        val videoClip = directed.timeline.videoTracks.head.clips.head
        val audioClip = directed.timeline.audioTracks.head.clips.head
        assert(Math.abs(videoClip.duration - 6.5) < 0.001)
        assert(audioClip.duration == 6.2)
        assert(videoClip.startTime == audioClip.startTime)

  test("AIDirectorService skips AudioGeneratorCapability and creates no AudioClip when narration is blank"):
    val audioCallCount = new AtomicInteger(0)

    given StoryboardPlannerCapability with
      override def createStoryboard(story: String)(using Label[AIDirectorError]): StoryboardPlan =
        StoryboardPlan(
          styleBible = "Silent film style",
          scenes = List(
            SceneDirective(1, "Wide", "   ", "Empty street at night", 4.0)
          )
        )

    given ImageGeneratorCapability with
      override def generateImage(prompt: String)(using Label[AIGenerationError]): GeneratedImage =
        GeneratedImage("/tmp/silent.png", 1024, 576, prompt)

    given AudioGeneratorCapability with
      override def generateSpeech(text: String, voiceIdOpt: Option[String])(using Label[AIGenerationError]): GeneratedAudio =
        audioCallCount.incrementAndGet()
        GeneratedAudio("/tmp/should_not_exist.mp3", 2.0, text)

    val result = Await.result(AIDirectorService.generateAndAppendStory("Silent scene", Timeline.default), 5.seconds)

    result match
      case Left(err) => fail(s"Assembly failed unexpectedly: $err")
      case Right(directed) =>
        assert(audioCallCount.get() == 0, "AudioGeneratorCapability must not be invoked for blank narration")
        assert(directed.timeline.videoTracks.head.clips.size == 1)
        assert(directed.timeline.audioTracks.head.clips.isEmpty)

  test("AIDirectorService propagates TTS errors and aborts workflow without mutating the timeline"):
    given StoryboardPlannerCapability with
      override def createStoryboard(story: String)(using Label[AIDirectorError]): StoryboardPlan =
        StoryboardPlan(
          styleBible = "Documentary",
          scenes = List(
            SceneDirective(1, "Medium", "Valid narrated text", "Narrator studio", 5.0)
          )
        )

    given ImageGeneratorCapability with
      override def generateImage(prompt: String)(using Label[AIGenerationError]): GeneratedImage =
        GeneratedImage("/tmp/doc.png", 1024, 576, prompt)

    given AudioGeneratorCapability with
      override def generateSpeech(text: String, voiceIdOpt: Option[String])(using Label[AIGenerationError]): GeneratedAudio =
        break(AIGenerationError("Speech synthesis failed: voice unavailable"))

    val initialTimeline = Timeline.default
    val result = Await.result(AIDirectorService.generateAndAppendStory("TTS error test", initialTimeline), 5.seconds)

    assert(result == Left(AIGenerationError("Speech synthesis failed: voice unavailable")))
    assert(initialTimeline.videoTracks.head.clips.isEmpty)
    assert(initialTimeline.audioTracks.head.clips.isEmpty)

  test("AIDirectorService applies FadeIn on opening scene and shot-calibrated ZoomIn effects thereafter"):
    given StoryboardPlannerCapability with
      override def createStoryboard(story: String)(using Label[AIDirectorError]): StoryboardPlan =
        StoryboardPlan(
          styleBible = "Cinematic",
          scenes = List(
            SceneDirective(1, "EstablishingWide", "   ", "Opening landscape", 4.0),
            SceneDirective(2, "ActionMedium", "   ", "Hero running", 4.0),
            SceneDirective(3, "CloseUp", "   ", "Hero face", 4.0),
            SceneDirective(4, "WideShot", "   ", "Explicit effect scene", 4.0, effect = VideoEffect.Sepia)
          )
        )

    given ImageGeneratorCapability with
      override def generateImage(prompt: String)(using Label[AIGenerationError]): GeneratedImage =
        GeneratedImage("/tmp/scene.png", 1024, 576, prompt)

    val result = Await.result(AIDirectorService.generateAndAppendStory("Camera effects test", Timeline.default), 5.seconds)

    result match
      case Left(err) => fail(s"Assembly failed: $err")
      case Right(directed) =>
        val clips = directed.timeline.videoTracks.head.clips
        assert(clips.size == 4)
        assert(clips(0).effect == VideoEffect.FadeIn(fadeDuration = 1.0))
        assert(clips(1).effect == VideoEffect.ZoomIn(targetScale = 1.16))
        assert(clips(2).effect == VideoEffect.ZoomIn(targetScale = 1.07))
        assert(clips(3).effect == VideoEffect.Sepia)