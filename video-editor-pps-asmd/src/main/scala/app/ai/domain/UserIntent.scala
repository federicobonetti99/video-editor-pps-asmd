package app.ai.domain

enum TrackType:
  case Video, Audio

enum AdjustDirection:
  case Increase, Decrease, Set

sealed trait UserIntent

object UserIntent:
  case class GenerateImage(prompt: String) extends UserIntent
  case class GenerateSpeech(text: String) extends UserIntent
  case class CutClip(timestamp: Double) extends UserIntent
  case class ApplyEffect(effectName: String) extends UserIntent
  case class AdjustEffect(
                           direction: AdjustDirection,
                           parameter: Option[String] = None,
                           amount: Option[Double] = None
                         ) extends UserIntent
  case class AdjustAudioVolume(
                                direction: AdjustDirection,
                                value: Option[Double] = None
                              ) extends UserIntent
  case class AddTrack(trackType: TrackType) extends UserIntent
  case object RemoveSilences extends UserIntent
  case object Undo extends UserIntent
  case class Unknown(rawText: String) extends UserIntent