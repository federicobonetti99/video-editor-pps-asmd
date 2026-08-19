package core.model

sealed trait MediaClip:
  def sourceUrl: String
  def sourceLength: Double
  def startTime: Double
  def trimStart: Double
  def duration: Double
  def withTimes(newStartTime: Double, newTrimStart: Double, newDuration: Double): MediaClip

case class VideoClip(
                      sourceUrl: String,
                      sourceLength: Double,
                      startTime: Double,
                      trimStart: Double,
                      duration: Double,
                      effect: VideoEffect = VideoEffect.None
                    ) extends MediaClip:
  override def withTimes(newStartTime: Double, newTrimStart: Double, newDuration: Double): VideoClip =
    this.copy(startTime = newStartTime, trimStart = newTrimStart, duration = newDuration)

case class AudioClip(
                      sourceUrl: String,
                      sourceLength: Double,
                      startTime: Double,
                      trimStart: Double,
                      duration: Double,
                      volumePoints: List[(Double, Double)] = List.empty
                    ) extends MediaClip:
  override def withTimes(newStartTime: Double, newTrimStart: Double, newDuration: Double): AudioClip =
    this.copy(startTime = newStartTime, trimStart = newTrimStart, duration = newDuration)

case class VideoTrack(id: Int, clips: List[VideoClip])
case class AudioTrack(id: Int, clips: List[AudioClip])

case class Timeline(
                     videoTracks: List[VideoTrack],
                     audioTracks: List[AudioTrack],
                     currentTime: Double = 0.0
                   )

extension [C <: MediaClip](clip: C)
  def containsTime(time: Double): Boolean =
    time >= clip.startTime && time < (clip.startTime + clip.duration)

  def isSameAs(other: MediaClip): Boolean =
    clip.sourceUrl == other.sourceUrl && Math.abs(clip.startTime - other.startTime) < 0.001

  def relativeTimeAt(globalTime: Double): Double =
    (globalTime - clip.startTime) + clip.trimStart

  def endTime: Double =
    clip.startTime + clip.duration