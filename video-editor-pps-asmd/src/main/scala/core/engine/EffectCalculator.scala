package core.engine

import core.model.VideoEffect

case class EffectTransform(
                            scale: Double = 1.0,
                            translateX: Double = 0.0,
                            translateY: Double = 0.0,
                            opacity: Double = 1.0
                          )

trait EffectCalculator[E]:
  def compute(effect: E, relativeTime: Double, clipDuration: Double): EffectTransform

object EffectCalculator:

  extension [E](effect: E)(using calc: EffectCalculator[E])
    def computeTransform(relativeTime: Double, clipDuration: Double): EffectTransform =
      calc.compute(effect, relativeTime, clipDuration)

  given EffectCalculator[VideoEffect.ZoomIn] with
    def compute(eff: VideoEffect.ZoomIn, relativeTime: Double, clipDuration: Double): EffectTransform =
      val progress = if clipDuration > 0.0 then Math.max(0.0, Math.min(1.0, relativeTime / clipDuration)) else 0.0
      val currentScale = 1.0 + (eff.targetScale - 1.0) * progress
      EffectTransform(scale = currentScale)

  given EffectCalculator[VideoEffect.Shake] with
    def compute(eff: VideoEffect.Shake, relativeTime: Double, clipDuration: Double): EffectTransform =
      val dx = eff.intensity * Math.sin(2 * Math.PI * eff.freq * relativeTime)
      val dy = eff.intensity * Math.cos(2 * Math.PI * (eff.freq * 1.3) * relativeTime)
      EffectTransform(translateX = dx, translateY = dy)

  given EffectCalculator[VideoEffect.FadeIn] with
    def compute(eff: VideoEffect.FadeIn, relativeTime: Double, clipDuration: Double): EffectTransform =
      val progress = if eff.fadeDuration > 0.0 then Math.max(0.0, Math.min(1.0, relativeTime / eff.fadeDuration)) else 1.0
      EffectTransform(opacity = progress)

  given EffectCalculator[VideoEffect] with
    def compute(effect: VideoEffect, relativeTime: Double, clipDuration: Double): EffectTransform =
      effect match
        case z: VideoEffect.ZoomIn => summon[EffectCalculator[VideoEffect.ZoomIn]].compute(z, relativeTime, clipDuration)
        case s: VideoEffect.Shake  => summon[EffectCalculator[VideoEffect.Shake]].compute(s, relativeTime, clipDuration)
        case f: VideoEffect.FadeIn => summon[EffectCalculator[VideoEffect.FadeIn]].compute(f, relativeTime, clipDuration)
        case _                     => EffectTransform()

  def computeTransform(
                        effect: VideoEffect,
                        relativeTime: Double,
                        clipDuration: Double
                      ): EffectTransform =
    effect.computeTransform(relativeTime, clipDuration)