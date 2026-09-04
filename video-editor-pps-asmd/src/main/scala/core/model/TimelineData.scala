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

sealed trait VisualClip extends MediaClip:
  def effect: VideoEffect
  def withEffect(newEffect: VideoEffect): VisualClip

case class VideoClip(
                      sourceUrl: String,
                      sourceLength: Double,
                      timing: ClipTiming,
                      effect: VideoEffect = VideoEffect.None
                    ) extends VisualClip:
  override def withEffect(newEffect: VideoEffect): VideoClip = copy(effect = newEffect)

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

case class ImageClip(
                      sourceUrl: String,
                      sourceLength: Double = Double.PositiveInfinity,
                      timing: ClipTiming,
                      effect: VideoEffect = VideoEffect.None
                    ) extends VisualClip:
  override def withEffect(newEffect: VideoEffect): ImageClip = copy(effect = newEffect)

object ImageClip:
  def create(
              sourceUrl: String,
              startTime: Double,
              duration: Double = 5.0,
              effect: VideoEffect = VideoEffect.None
            ): ImageClip =
    ImageClip(
      sourceUrl = sourceUrl,
      sourceLength = Double.PositiveInfinity,
      timing = ClipTiming(startTime = startTime, trimStart = 0.0, duration = duration),
      effect = effect
    )

  def apply(
             sourceUrl: String,
             sourceLength: Double,
             startTime: Double,
             trimStart: Double,
             duration: Double
           ): ImageClip =
    ImageClip(
      sourceUrl = sourceUrl,
      sourceLength = sourceLength,
      timing = ClipTiming(startTime = startTime, trimStart = trimStart, duration = duration),
      effect = VideoEffect.None
    )

case class AudioClip(
                      sourceUrl: String,
                      sourceLength: Double,
                      timing: ClipTiming,
                      volume: Double = 1.0,
                      volumePoints: List[VolumePoint] = List.empty
                    ) extends MediaClip

object AudioClip:
  def apply(
             sourceUrl: String,
             sourceLength: Double,
             startTime: Double,
             trimStart: Double,
             duration: Double,
             volume: Double,
             volumePoints: List[VolumePoint]
           ): AudioClip =
    AudioClip(sourceUrl, sourceLength, ClipTiming(startTime, trimStart, duration), volume, volumePoints)

  def apply(
             sourceUrl: String,
             sourceLength: Double,
             startTime: Double,
             trimStart: Double,
             duration: Double,
             volumePoints: List[VolumePoint]
           ): AudioClip =
    AudioClip(sourceUrl, sourceLength, ClipTiming(startTime, trimStart, duration), 1.0, volumePoints)

  def apply(
             sourceUrl: String,
             sourceLength: Double,
             startTime: Double,
             trimStart: Double,
             duration: Double
           ): AudioClip =
    AudioClip(sourceUrl, sourceLength, ClipTiming(startTime, trimStart, duration), 1.0, List.empty)

sealed trait Track[+C <: MediaClip]:
  def id: Int
  def clips: List[C]

case class VideoTrack(id: Int, clips: List[VisualClip]) extends Track[VisualClip]
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
    val res = clip match
      case v: VideoClip => v.copy(timing = newTiming)
      case i: ImageClip => i.copy(timing = newTiming)
      case a: AudioClip => a.copy(timing = newTiming)
    res.asInstanceOf[C]

  def withTiming(newTiming: ClipTiming): C =
    val res = clip match
      case v: VideoClip => v.copy(timing = newTiming)
      case i: ImageClip => i.copy(timing = newTiming)
      case a: AudioClip => a.copy(timing = newTiming)
    res.asInstanceOf[C]

extension [V <: VisualClip](clip: V)
  def withEffect(newEffect: VideoEffect): V =
    val res = clip match
      case v: VideoClip => v.copy(effect = newEffect)
      case i: ImageClip => i.copy(effect = newEffect)
    res.asInstanceOf[V]