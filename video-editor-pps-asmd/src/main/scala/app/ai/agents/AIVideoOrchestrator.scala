package app.ai.agents

import app.ai.capabilities.{ImageGeneratorCapability, IntentParserCapability}
import app.ai.domain.*
import scala.util.boundary.{Label, break}

object AIVideoOrchestrator:

  def handleCommand(command: String)(using
                                     parser: IntentParserCapability,
                                     generator: ImageGeneratorCapability
  )(using Label[AIGenerationError], Label[AIParseError]): GeneratedImage =
    parser.parse(command) match
      case UserIntent.GenerateImage(prompt) =>
        generator.generateImage(prompt)
      case UserIntent.Unknown(raw) =>
        break(AIGenerationError(s"Comando non riconosciuto: $raw"))
      case _ =>
        break(AIGenerationError("Intento non supportato dall'assistente"))