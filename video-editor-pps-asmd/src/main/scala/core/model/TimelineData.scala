package core.model

case class ClipTiming(
                       startTime: Double,
                       trimStart: Double,
                       duration: Double
                     ):
  def endTime: Double = startTime + duration
  def relativeTimeAt(globalTime: Double): Double = (globalTime - startTime) + trimStart
  def contains(time: Double): Boolean = time >= startTime && time < endTime

case class VolumePoint(time: Double, volume: Double)

sealed trait MediaClip:
  def sourceUrl: String
  def sourceLength: Double
  def timing: ClipTiming

case class VideoClip(
                      sourceUrl: String,
                      sourceLength: Double,
                      timing: ClipTiming,
                      effect: VideoEffect = VideoEffect.None
                    ) extends MediaClip

object VideoClip:
  def apply(
             sourceUrl: String,
             sourceLength: Double,
             startTime: Double,
             trimStart: Double,
             duration: Double,
             effect: VideoEffect
           ): VideoClip =
    VideoClip(sourceUrl, sourceLength, ClipTiming(startTime, trimStart, duration), effect)

  def apply(
             sourceUrl: String,
             sourceLength: Double,
             startTime: Double,
             trimStart: Double,
             duration: Double
           ): VideoClip =
    VideoClip(sourceUrl, sourceLength, ClipTiming(startTime, trimStart, duration), VideoEffect.None)

case class AudioClip(
                      sourceUrl: String,
                      sourceLength: Double,
                      timing: ClipTiming,
                      volumePoints: List[VolumePoint] = List.empty
                    ) extends MediaClip

object AudioClip:
  def apply(
             sourceUrl: String,
             sourceLength: Double,
             startTime: Double,
             trimStart: Double,
             duration: Double,
             volumePoints: List[VolumePoint]
           ): AudioClip =
    AudioClip(sourceUrl, sourceLength, ClipTiming(startTime, trimStart, duration), volumePoints)

  def apply(
             sourceUrl: String,
             sourceLength: Double,
             startTime: Double,
             trimStart: Double,
             duration: Double
           ): AudioClip =
    AudioClip(sourceUrl, sourceLength, ClipTiming(startTime, trimStart, duration), List.empty)

sealed trait Track[+C <: MediaClip]:
  def id: Int
  def clips: List[C]

case class VideoTrack(id: Int, clips: List[VideoClip]) extends Track[VideoClip]
case class AudioTrack(id: Int, clips: List[AudioClip]) extends Track[AudioClip]

case class Timeline(
                     videoTracks: List[VideoTrack] = List.empty,
                     audioTracks: List[AudioTrack] = List.empty,
                     currentTime: Double = 0.0
                   )

object Timeline:
  def default: Timeline = Timeline(
    videoTracks = List(VideoTrack(id = 1, clips = Nil)),
    audioTracks = List(
      AudioTrack(id = 1, clips = Nil),
      AudioTrack(id = 2, clips = Nil)
    )
  )

extension [C <: MediaClip](clip: C)
  def startTime: Double = clip.timing.startTime
  def duration: Double = clip.timing.duration
  def trimStart: Double = clip.timing.trimStart
  def endTime: Double = clip.timing.endTime

  def containsTime(time: Double): Boolean = clip.timing.contains(time)
  def relativeTimeAt(globalTime: Double): Double = clip.timing.relativeTimeAt(globalTime)

  def isSameAs(other: MediaClip): Boolean =
    clip.sourceUrl == other.sourceUrl && Math.abs(clip.timing.startTime - other.timing.startTime) < 0.001

  def withTimes(newStartTime: Double, newTrimStart: Double, newDuration: Double): C =
    val newTiming = ClipTiming(newStartTime, newTrimStart, newDuration)
    clip match
      case v: VideoClip => v.copy(timing = newTiming).asInstanceOf[C]
      case a: AudioClip => a.copy(timing = newTiming).asInstanceOf[C]

  def withTiming(newTiming: ClipTiming): C = clip match
    case v: VideoClip => v.copy(timing = newTiming).asInstanceOf[C]
    case a: AudioClip => a.copy(timing = newTiming).asInstanceOf[C]