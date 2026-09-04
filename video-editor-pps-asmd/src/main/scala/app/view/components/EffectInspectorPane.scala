package view

import scalafx.Includes.*
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.control.{Label, TextField}
import scalafx.geometry.{Insets, Pos}
import core.model.{AudioClip, MediaClip, VisualClip, VideoEffect}

class EffectInspectorPane(
                           onEffectChanged: VideoEffect => Unit,
                           onAudioVolumeChanged: Double => Unit = _ => ()
                         ) extends VBox:
  spacing = 4
  padding = Insets(2)
  alignment = Pos.TopLeft
  minWidth = 0.0
  prefWidth = 0.0
  maxWidth = Double.MaxValue
  style = "-fx-background-color: transparent;"

  def updateSelection(clipOpt: Option[MediaClip]): Unit =
    children.clear()

    clipOpt match
      case None =>
        ()

      case Some(videoClip: VisualClip) =>
        renderControlsFor(videoClip.effect, videoClip.timing.duration)

      case Some(audioClip: AudioClip) =>
        renderAudioControls(audioClip.volume)

      case Some(_) =>
        ()

  private def renderAudioControls(currentVolume: Double): Unit =
    val rowVol = createNumberInput("Vol:", currentVolume, min = 0.0, max = 2.0) { newVal =>
      onAudioVolumeChanged(newVal)
    }
    children.add(rowVol)

  private def renderControlsFor(effect: VideoEffect, maxDuration: Double): Unit =
    effect match
      case VideoEffect.ZoomIn(targetScale) =>
        val inputRow = createNumberInput("Zoom:", targetScale, min = 1.0, max = 5.0) { newVal =>
          onEffectChanged(VideoEffect.ZoomIn(targetScale = newVal))
        }
        children.add(inputRow)

      case VideoEffect.Shake(intensity, freq) =>
        val rowInt = createNumberInput("Int:", intensity, min = 1.0, max = 100.0) { newVal =>
          onEffectChanged(VideoEffect.Shake(intensity = newVal, freq = freq))
        }
        val rowFreq = createNumberInput("Hz:", freq, min = 0.1, max = 20.0) { newVal =>
          onEffectChanged(VideoEffect.Shake(intensity = intensity, freq = newVal))
        }
        children.addAll(rowInt, rowFreq)

      case VideoEffect.FadeIn(fadeDuration) =>
        val rowDur = createNumberInput("Fade:", fadeDuration, min = 0.1, max = Math.max(0.2, maxDuration)) { newVal =>
          onEffectChanged(VideoEffect.FadeIn(fadeDuration = newVal))
        }
        children.add(rowDur)

      case _ =>
        ()

  private def createNumberInput(
                                 label: String,
                                 initial: Double,
                                 min: Double,
                                 max: Double
                               )(onCommit: Double => Unit): HBox =
    val lbl = new Label(label):
      minWidth = 0.0
      prefWidth = 30.0
      style = "-fx-text-fill: #aaaaaa; -fx-font-size: 9px;"

    val field = new TextField:
      text = f"$initial%.2f"
      prefWidth = 36.0
      maxWidth = 40.0
      minWidth = 0.0
      style = "-fx-background-color: #2b2b2b; -fx-text-fill: #ffffff; -fx-font-size: 9px; -fx-padding: 1 2 1 2; -fx-background-radius: 2;"

    def validateAndCommit(): Unit =
      field.text.value.toDoubleOption match
        case Some(v) =>
          val clamped = Math.max(min, Math.min(max, v))
          field.text = f"$clamped%.2f"
          onCommit(clamped)
        case None =>
          field.text = f"$initial%.2f"

    field.onAction = _ => validateAndCommit()
    field.focused.onChange { (_, _, focused) =>
      if !focused then validateAndCommit()
    }

    new HBox(2):
      alignment = Pos.CenterLeft
      minWidth = 0.0
      children = Seq(lbl, field)