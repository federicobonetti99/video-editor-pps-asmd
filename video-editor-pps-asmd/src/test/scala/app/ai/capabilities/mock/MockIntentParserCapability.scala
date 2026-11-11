package app.ai.capabilities.mock

import app.ai.capabilities.IntentParserCapability
import app.ai.domain.{AIParseError, UserIntent}
import scala.util.boundary.{Label, break}

object MockIntentParserCapability extends IntentParserCapability:

  override def parse(text: String)(using Label[AIParseError]): UserIntent =
    val trimmed = text.trim
    val lower = trimmed.toLowerCase
    if trimmed.isEmpty then
      break(AIParseError("Il comando non puo' essere vuoto"))
    else if lower.contains("immagine") || lower.contains("foto") || lower.startsWith("genera") then
      UserIntent.GenerateImage(trimmed)
    else
      UserIntent.Unknown(trimmed)