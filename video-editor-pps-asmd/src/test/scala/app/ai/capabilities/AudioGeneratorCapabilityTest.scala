package app.ai.capabilities

import app.ai.capabilities.mock.MockAudioGeneratorCapability
import app.ai.domain.{AIGenerationError, GeneratedAudio}
import org.scalatest.funsuite.AnyFunSuite
import scala.util.boundary

class AudioGeneratorCapabilityTest extends AnyFunSuite:

  test("generate valid audio metadata for a narration prompt"):
    val result = boundary[AIGenerationError | GeneratedAudio]:
      MockAudioGeneratorCapability.generateSpeech("Benvenuti in questa storia", Some("it-IT-DiegoNeural"))

    result match
      case audio: GeneratedAudio =>
        assert(audio.path.nonEmpty)
        assert(audio.durationSeconds > 0.0)
        assert(audio.prompt == "Benvenuti in questa storia")
      case _ => fail("Dovrebbe generare una GeneratedAudio valida")

  test("fail with AIGenerationError when narration text is empty or blank"):
    val result = boundary[AIGenerationError | GeneratedAudio]:
      MockAudioGeneratorCapability.generateSpeech("   ", None)

    assert(result.isInstanceOf[AIGenerationError])