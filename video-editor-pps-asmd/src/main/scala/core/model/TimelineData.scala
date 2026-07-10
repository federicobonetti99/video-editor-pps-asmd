package core.model

sealed trait MediaClip:
  def sourceUrl: String
  def sourceLength: Double
  def startTime: Double
  def trimStart: Double
  def duration: Double

case class VideoClip(
    sourceUrl: String,
    sourceLength: Double,
    startTime: Double,
    trimStart: Double,
    duration: Double,
    effect: VideoEffect
  ) extends MediaClip

case class AudioClip(
    sourceUrl: String,
    sourceLength: Double,
    startTime: Double,
    trimStart: Double,
    duration: Double,
    volumePoints: List[(Double, Double)]
  ) extends MediaClip

enum VideoEffect:
  case None
  case FadeIn(duration: Double)
  case BlackAndWhite

case class VideoTrack(id: Int, clips: List[VideoClip])
case class AudioTrack(id: Int, clips: List[AudioClip])

case class Timeline(
     videoTracks: List[VideoTrack],
     audioTracks: List[AudioTrack],
     currentTime: Double = 0.0
   )