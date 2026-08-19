package core.model

enum VideoEffect:
  case None
  case Grayscale
  case Sepia
  case Invert
  case Brightness(level: Double = 0.2)
  case ZoomIn(targetScale: Double = 1.3)
  case Shake(intensity: Double = 8.0, freq: Double = 12.0)
  case FadeIn(fadeDuration: Double = 1.0)