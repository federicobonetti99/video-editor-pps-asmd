package app.ai.capabilities.mock

import app.ai.capabilities.AudioGeneratorCapability
import app.ai.domain.{AIGenerationError, GeneratedAudio}
import scala.util.boundary.{Label, break}

object MockAudioGeneratorCapability extends AudioGeneratorCapability:

  override def generateSpeech(text: String, voiceIdOpt: Option[String])(using Label[AIGenerationError]): GeneratedAudio =
    if text.trim.isEmpty then
      break(AIGenerationError("Il testo da sintetizzare non puo' essere vuoto"))
    else
      val normalized = text.trim
      val estimatedDuration = Math.max(1.5, normalized.split("\\s+").length * 0.4)
      GeneratedAudio(
        path = s"/tmp/mock_speech_${normalized.hashCode.abs}.mp3",
        durationSeconds = estimatedDuration,
        prompt = normalized
      )