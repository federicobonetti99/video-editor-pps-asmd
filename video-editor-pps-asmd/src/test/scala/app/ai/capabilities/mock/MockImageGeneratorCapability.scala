package app.ai.capabilities.mock

import app.ai.capabilities.ImageGeneratorCapability
import app.ai.domain.{AIGenerationError, GeneratedImage}
import scala.util.boundary.{Label, break}

object MockImageGeneratorCapability extends ImageGeneratorCapability:

  override def generateImage(prompt: String)(using Label[AIGenerationError]): GeneratedImage =
    if prompt.trim.isEmpty then
      break(AIGenerationError("Il prompt non puo' essere vuoto"))
    else
      GeneratedImage(
        path = s"/tmp/mock_gen_${prompt.trim.hashCode.abs}.png",
        width = 1920,
        height = 1080,
        prompt = prompt.trim
      )