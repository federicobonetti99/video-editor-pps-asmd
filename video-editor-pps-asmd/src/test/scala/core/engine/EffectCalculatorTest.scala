package core.engine

import org.scalatest.funsuite.AnyFunSuite
import core.model.VideoEffect
import core.engine.EffectCalculator.*

class EffectCalculatorTest extends AnyFunSuite:

  test("ZoomIn scale progression and clamping"):
    val zoom = VideoEffect.ZoomIn(targetScale = 2.0)

    val start = zoom.computeTransform(relativeTime = 0.0, clipDuration = 10.0)
    assert(start.scale == 1.0)

    val mid = zoom.computeTransform(relativeTime = 5.0, clipDuration = 10.0)
    assert(mid.scale == 1.5)

    val end = zoom.computeTransform(relativeTime = 10.0, clipDuration = 10.0)
    assert(end.scale == 2.0)

    val overTime = zoom.computeTransform(relativeTime = 15.0, clipDuration = 10.0)
    assert(overTime.scale == 2.0)

    val zeroDuration = zoom.computeTransform(relativeTime = 5.0, clipDuration = 0.0)
    assert(zeroDuration.scale == 1.0)

  test("Shake displacement"):
    val shake = VideoEffect.Shake(intensity = 10.0, freq = 2.0)
    val t0 = shake.computeTransform(relativeTime = 0.0, clipDuration = 5.0)
    assert(Math.abs(t0.translateX - 0.0) < 0.001)
    assert(Math.abs(t0.translateY - 10.0) < 0.001)

  test("FadeIn opacity progression"):
    val fade = VideoEffect.FadeIn(fadeDuration = 2.0)

    val start = fade.computeTransform(relativeTime = 0.0, clipDuration = 10.0)
    assert(Math.abs(start.opacity - 0.0) < 0.001)

    val mid = fade.computeTransform(relativeTime = 1.0, clipDuration = 10.0)
    assert(Math.abs(mid.opacity - 0.5) < 0.001)

    val end = fade.computeTransform(relativeTime = 2.5, clipDuration = 10.0)
    assert(Math.abs(end.opacity - 1.0) < 0.001)

  test("Neutral transform for static effects"):
    val staticEffects = List(
      VideoEffect.None,
      VideoEffect.Grayscale,
      VideoEffect.Sepia,
      VideoEffect.Invert,
      VideoEffect.Brightness(1.5)
    )
    for effect <- staticEffects do
      val res = effect.computeTransform(relativeTime = 3.0, clipDuration = 6.0)
      assert(res == EffectTransform(scale = 1.0, translateX = 0.0, translateY = 0.0, opacity = 1.0))

  test("Dispatch through generic VideoEffect ADT"):
    val genericEffect: VideoEffect = VideoEffect.ZoomIn(targetScale = 3.0)
    val res = genericEffect.computeTransform(relativeTime = 5.0, clipDuration = 10.0)
    assert(res.scale == 2.0)

  test("Backward compatible companion method"):
    val effect: VideoEffect = VideoEffect.FadeIn(fadeDuration = 4.0)
    val res = EffectCalculator.computeTransform(effect, relativeTime = 2.0, clipDuration = 10.0)
    assert(Math.abs(res.opacity - 0.5) < 0.001)