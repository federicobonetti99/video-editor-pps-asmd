package app.ai.capabilities.http

import app.ai.capabilities.AudioGeneratorCapability
import app.ai.domain.{AIGenerationError, GeneratedAudio}

import java.nio.file.{Files, Path}
import scala.sys.process.*
import scala.util.Try
import scala.util.boundary.{Label, break}

class EdgeAudioGeneratorCapability(
                                    defaultVoice: String = "it-IT-DiegoNeural"
                                  ) extends AudioGeneratorCapability:

  override def generateSpeech(
                               text: String,
                               voiceIdOpt: Option[String]
                             )(using Label[AIGenerationError]): GeneratedAudio =
    val cleanText = text.trim
    if cleanText.isEmpty then
      break(AIGenerationError("Il testo da sintetizzare non può essere vuoto."))

    val selectedVoice = voiceIdOpt.map(_.trim).filter(_.nonEmpty).getOrElse(defaultVoice)

    val tempFile: Path = Files.createTempFile("tts_edge_", ".mp3")
    tempFile.toFile.deleteOnExit()

    val cmd = Seq(
      "edge-tts",
      "--voice", selectedVoice,
      "--text", cleanText,
      "--write-media", tempFile.toAbsolutePath.toString
    )

    val exitCode = Try(cmd.!).getOrElse(-1)

    if exitCode != 0 || !Files.exists(tempFile) || Files.size(tempFile) == 0L then
      break(AIGenerationError("Errore durante la generazione vocale con edge-tts (verifica che sia installato e nel PATH)."))

    GeneratedAudio(
      path = tempFile.toAbsolutePath.toString,
      durationSeconds = estimateDuration(cleanText),
      prompt = cleanText
    )

  private def estimateDuration(text: String): Double =
    val words = text.split("\\s+").count(_.nonEmpty)
    Math.max(1.5, words * 0.45)