package app.ai

import app.ai.agents.QuickCommandOrchestrator
import app.ai.capabilities.{ImageGeneratorCapability, IntentParserCapability}
import app.ai.domain.*
import app.ai.services.{AIActionOutcome, AITimelineService}
import core.model.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.util.boundary
import scala.util.boundary.break

class AITimelineFlowTest extends AnyFunSuite with Matchers:

  test("GeneratedImage calcola correttamente l'aspect ratio 16:9 e gestisce height = 0") {
    val widescreen = GeneratedImage("/tmp/img.png", width = 1024, height = 576, prompt = "test")
    widescreen.aspectRatio shouldBe (16.0 / 9.0 +- 0.001)

    val invalid = GeneratedImage("/tmp/broken.png", width = 1024, height = 0, prompt = "test")
    invalid.aspectRatio shouldBe 0.0
  }

  test("AITimelineService inserisce una ImageClip al cursore corrente della timeline") {
    given IntentParserCapability with
      override def parse(text: String)(using boundary.Label[AIParseError]): UserIntent =
        UserIntent.GenerateImage("cinematic elephant in savannah, widescreen")

    given ImageGeneratorCapability with
      override def generateImage(prompt: String)(using boundary.Label[AIGenerationError]): GeneratedImage =
        GeneratedImage(path = "/tmp/elephant_16x9.png", width = 1024, height = 576, prompt = prompt)

    val initialTimeline = Timeline.default.copy(currentTime = 4.5)

    val outcome = boundary[AIGenerationError | AIParseError | AIActionOutcome]:
      AITimelineService.processAndApply(
        command = "genera un elefante",
        timeline = initialTimeline,
        trackId = 1,
        defaultDuration = 6.0
      )

    outcome match
      case AIActionOutcome.UpdatedTimeline(updated) =>
        val clips = updated.videoTracks.find(_.id == 1).map(_.clips).getOrElse(Nil)
        clips should have size 1
        clips.head.startTime shouldBe 4.5
        clips.head.duration shouldBe 6.0
      case other =>
        fail(s"Atteso AIActionOutcome.UpdatedTimeline, ricevuto: $other")
  }

  test("QuickCommandOrchestrator propaga AIParseError senza invocare il generatore di immagini") {
    given IntentParserCapability with
      override def parse(text: String)(using boundary.Label[AIParseError]): UserIntent =
        break(AIParseError("Errore di parsing simulato"))

    given ImageGeneratorCapability with
      override def generateImage(prompt: String)(using boundary.Label[AIGenerationError]): GeneratedImage =
        fail("ImageGeneratorCapability non deve essere chiamata se il parsing fallisce")

    val outcome = boundary[AIGenerationError | AIParseError | GeneratedImage]:
      QuickCommandOrchestrator.handleCommand("comando malformato")

    outcome shouldBe AIParseError("Errore di parsing simulato")
  }

  test("QuickCommandOrchestrator restituisce AIGenerationError se l'intento è Unknown") {
    given IntentParserCapability with
      override def parse(text: String)(using boundary.Label[AIParseError]): UserIntent =
        UserIntent.Unknown(text)

    given ImageGeneratorCapability with
      override def generateImage(prompt: String)(using boundary.Label[AIGenerationError]): GeneratedImage =
        fail("ImageGeneratorCapability non deve essere chiamata per intenti Unknown")

    val outcome = boundary[AIGenerationError | AIParseError | GeneratedImage]:
      QuickCommandOrchestrator.handleCommand("raccontami una barzelletta")

    outcome shouldBe a[AIGenerationError]
  }