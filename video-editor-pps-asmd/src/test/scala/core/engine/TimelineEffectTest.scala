package core.engine

import org.scalatest.funsuite.AnyFunSuite
import core.model.*
import org.scalatest.matchers.should.Matchers.shouldBe

class TimelineEffectTest extends AnyFunSuite:

  private val baseClip = VideoClip(
    sourceUrl = "file://video.mp4",
    sourceLength = 10.0,
    startTime = 0.0,
    trimStart = 0.0,
    duration = 10.0,
    effect = VideoEffect.None
  )

  private val baseTimeline = Timeline(
    videoTracks = List(VideoTrack(id = 1, clips = List(baseClip))),
    audioTracks = Nil
  )

  test("Applying an effect to a video clip updates the effect and maintains timeline immutability") {
    val updatedTimeline = TimelineEngine.applyEffectToVideoClip(
      baseTimeline,
      trackId = 1,
      clipIndex = 0,
      effect = VideoEffect.Grayscale
    )

    val updatedClip = updatedTimeline.videoTracks.head.clips.head
    assert(updatedClip.effect == VideoEffect.Grayscale)
    assert(baseTimeline.videoTracks.head.clips.head.effect == VideoEffect.None)
  }

  test("Applying an effect to a non-existent track or index leaves the timeline unchanged") {
    val nonExistentTrack = TimelineEngine.applyEffectToVideoClip(
      baseTimeline,
      trackId = 99,
      clipIndex = 0,
      effect = VideoEffect.Sepia
    )
    val outOfBoundsIndex = TimelineEngine.applyEffectToVideoClip(
      baseTimeline,
      trackId = 1,
      clipIndex = 5,
      effect = VideoEffect.Sepia
    )

    assert(nonExistentTrack == baseTimeline)
    assert(outOfBoundsIndex == baseTimeline)
  }

  test("ZoomIn effect interpolates scale correctly from start to end") {
    val effect = VideoEffect.ZoomIn(targetScale = 2.0)
    val duration = 10.0

    val transformStart = EffectCalculator.computeTransform(effect, relativeTime = 0.0, clipDuration = duration)
    val transformMid = EffectCalculator.computeTransform(effect, relativeTime = 5.0, clipDuration = duration)
    val transformEnd = EffectCalculator.computeTransform(effect, relativeTime = 10.0, clipDuration = duration)

    assert(transformStart.scale == 1.0)
    assert(transformMid.scale == 1.5)
    assert(transformEnd.scale == 2.0)
  }

  test("ZoomIn effect clamps scale when relativeTime exceeds clip duration") {
    val effect = VideoEffect.ZoomIn(targetScale = 2.0)
    val transform = EffectCalculator.computeTransform(effect, relativeTime = 15.0, clipDuration = 10.0)

    assert(transform.scale == 2.0)
  }

  test("FadeIn effect computes correct opacity gradient") {
    val effect = VideoEffect.FadeIn(fadeDuration = 2.0)
    val duration = 10.0

    val start = EffectCalculator.computeTransform(effect, relativeTime = 0.0, clipDuration = duration)
    val mid = EffectCalculator.computeTransform(effect, relativeTime = 1.0, clipDuration = duration)
    val complete = EffectCalculator.computeTransform(effect, relativeTime = 2.0, clipDuration = duration)
    val beyond = EffectCalculator.computeTransform(effect, relativeTime = 5.0, clipDuration = duration)

    assert(start.opacity == 0.0)
    assert(mid.opacity == 0.5)
    assert(complete.opacity == 1.0)
    assert(beyond.opacity == 1.0)
  }

  test("Shake effect calculates non-zero coordinate offsets during playback") {
    val effect = VideoEffect.Shake(intensity = 10.0, freq = 5.0)
    val transformAtQuarterSec = EffectCalculator.computeTransform(effect, relativeTime = 0.05, clipDuration = 10.0)

    assert(transformAtQuarterSec.translateX != 0.0)
    assert(transformAtQuarterSec.translateY != 0.0)
  }

  test("Applying an effect to an image clip updates the effect on the image") {
    val imageClip = ImageClip.create(
      sourceUrl = "file://image.png",
      startTime = 0.0,
      duration = 5.0
    )
    val timelineWithImage = Timeline(
      videoTracks = List(VideoTrack(id = 1, clips = List(imageClip))),
      audioTracks = Nil
    )
  
    val updatedTimeline = TimelineEngine.applyEffectToVideoClip(
      timelineWithImage,
      trackId = 1,
      clipIndex = 0,
      effect = VideoEffect.Sepia
    )
  
    val updatedClip = updatedTimeline.videoTracks.head.clips.head
    assert(updatedClip.isInstanceOf[ImageClip])
    assert(updatedClip.effect == VideoEffect.Sepia)
  }