package app.ai.capabilities

import app.ai.domain.{AIGenerationError, GeneratedAudio}
import scala.util.boundary.Label

trait AudioGeneratorCapability:
  def generateSpeech(text: String, voiceIdOpt: Option[String] = None)(using Label[AIGenerationError]): GeneratedAudio