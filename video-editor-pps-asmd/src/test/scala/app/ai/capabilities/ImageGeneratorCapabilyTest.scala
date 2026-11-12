package app.ai.capabilities

import org.scalatest.funsuite.AnyFunSuite
import app.ai.capabilities.mock.MockImageGeneratorCapability
import app.ai.domain.{AIGenerationError, GeneratedImage}
import scala.util.boundary

class ImageGeneratorCapabilityTest extends AnyFunSuite:

  test("generate valid image metadata for a prompt"):
    val result = boundary[AIGenerationError | GeneratedImage]:
      MockImageGeneratorCapability.generateImage("un tramonto sul mare")

    result match
      case img: GeneratedImage =>
        assert(img.path.nonEmpty)
        assert(img.width == 1920)
        assert(img.height == 1080)
      case _ => fail("Dovrebbe generare una GeneratedImage valida")