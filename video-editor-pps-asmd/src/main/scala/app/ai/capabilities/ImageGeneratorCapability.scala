package app.ai.capabilities

import app.ai.domain.{AIGenerationError, GeneratedImage}
import scala.util.boundary.Label

trait ImageGeneratorCapability:
  def generateImage(prompt: String)(using Label[AIGenerationError]): GeneratedImage