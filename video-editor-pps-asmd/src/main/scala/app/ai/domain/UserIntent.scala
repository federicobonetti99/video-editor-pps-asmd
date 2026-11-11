package app.ai.domain

sealed trait UserIntent

object UserIntent:
  case class GenerateImage(prompt: String) extends UserIntent
  case class CutClip(timestamp: Double) extends UserIntent
  case class ApplyEffect(effectName: String) extends UserIntent
  case object Undo extends UserIntent
  case class Unknown(rawText: String) extends UserIntent