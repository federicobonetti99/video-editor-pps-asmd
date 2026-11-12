package app.ai.services

import app.ai.agents.AIVideoOrchestrator
import app.ai.capabilities.{ImageGeneratorCapability, IntentParserCapability}
import app.ai.domain.*
import core.engine.TimelineEngine
import core.model.{ImageClip, Timeline}
import scala.util.boundary.Label

object AITimelineService:

  def processAndApply(
                       command: String,
                       timeline: Timeline,
                       trackId: Int = 1,
                       defaultDuration: Double = 5.0
                     )(using
                       parser: IntentParserCapability,
                       generator: ImageGeneratorCapability
                     )(using Label[AIGenerationError], Label[AIParseError]): Timeline =
    val generatedImage = AIVideoOrchestrator.handleCommand(command)
    val clip = ImageClip.create(
      sourceUrl = generatedImage.path,
      startTime = timeline.currentTime,
      duration = defaultDuration
    )
    TimelineEngine.addVideoClip(timeline, trackId, clip)