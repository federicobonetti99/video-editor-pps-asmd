package core.engine

import core.model.VideoEffect

case class EffectTransform(
                            scale: Double = 1.0,
                            translateX: Double = 0.0,
                            translateY: Double = 0.0,
                            opacity: Double = 1.0
                          )

object EffectCalculator:

  def computeTransform(
                        effect: VideoEffect,
                        relativeTime: Double,
                        clipDuration: Double
                      ): EffectTransform =
    effect match
      case VideoEffect.None | VideoEffect.Grayscale | VideoEffect.Sepia | VideoEffect.Invert | VideoEffect.Brightness(_) =>
        EffectTransform()

      case VideoEffect.ZoomIn(targetScale) =>
        val progress = if clipDuration > 0.0 then Math.max(0.0, Math.min(1.0, relativeTime / clipDuration)) else 0.0
        val currentScale = 1.0 + (targetScale - 1.0) * progress
        EffectTransform(scale = currentScale)

      case VideoEffect.Shake(intensity, freq) =>
        val dx = intensity * Math.sin(2 * Math.PI * freq * relativeTime)
        val dy = intensity * Math.cos(2 * Math.PI * (freq * 1.3) * relativeTime)
        EffectTransform(translateX = dx, translateY = dy)

      case VideoEffect.FadeIn(fadeDuration) =>
        val progress = if fadeDuration > 0.0 then Math.max(0.0, Math.min(1.0, relativeTime / fadeDuration)) else 1.0
        EffectTransform(opacity = progress)