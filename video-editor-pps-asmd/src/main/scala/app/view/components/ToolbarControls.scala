package app.view.components

import core.model.VideoEffect
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Pos
import scalafx.scene.control.{Button, ComboBox, Label}
import scalafx.scene.layout.HBox

class ToolbarControls(
                       onImport: () => Unit,
                       onDelete: () => Unit,
                       onCut: () => Unit,
                       onSnap: () => Unit,
                       onPlay: () => Unit,
                       onEffectSelected: VideoEffect => Unit
                     ) extends HBox:

  spacing = 10
  alignment = Pos.CenterLeft

  private val importButton = new Button("Import"):
    focusTraversable = false
    onAction = _ => onImport()

  private val deleteButton = new Button("Delete"):
    focusTraversable = false
    onAction = _ => onDelete()

  private val cutButton = new Button("Cut"):
    focusTraversable = false
    onAction = _ => onCut()

  private val snapButton = new Button("Snap"):
    focusTraversable = false
    onAction = _ => onSnap()

  private val playButton = new Button("Play/Pause"):
    focusTraversable = false
    onAction = _ => onPlay()

  private val timeLabel = new Label("00:00.000"):
    style = "-fx-text-fill: white; -fx-font-family: monospace; -fx-font-size: 14px;"

  private val effectLabel = new Label("Effect:"):
    style = "-fx-text-fill: white; -fx-font-size: 13px;"

  private val effectOptions = Seq(
    "None"       -> VideoEffect.None,
    "Grayscale"  -> VideoEffect.Grayscale,
    "Sepia"      -> VideoEffect.Sepia,
    "Invert"     -> VideoEffect.Invert,
    "Brightness" -> VideoEffect.Brightness(0.25),
    "ZoomIn"     -> VideoEffect.ZoomIn(1.3),
    "Shake"      -> VideoEffect.Shake(8.0, 12.0),
    "FadeIn"     -> VideoEffect.FadeIn(1.0)
  )

  private var isUpdatingUi = false

  private val effectComboBox = new ComboBox[String](ObservableBuffer(effectOptions.map(_._1)*)):
    focusTraversable = false
    value = "None"
    onAction = _ =>
      if !isUpdatingUi && value.value != null then
        effectOptions.find(_._1 == value.value).foreach { case (_, eff) =>
          onEffectSelected(eff)
        }

  children = Seq(
    importButton,
    deleteButton,
    cutButton,
    snapButton,
    playButton,
    timeLabel,
    effectLabel,
    effectComboBox
  )

  def updateTimeLabel(seconds: Double): Unit =
    val totalSeconds = seconds.toInt
    val minutes = totalSeconds / 60
    val remSeconds = totalSeconds % 60
    val millis = ((seconds - totalSeconds) * 1000).toInt
    timeLabel.text = f"$minutes%02d:$remSeconds%02d.$millis%03d"

  def setSelectedEffect(effect: VideoEffect): Unit =
    isUpdatingUi = true
    val name = effect match
      case VideoEffect.None          => "None"
      case VideoEffect.Grayscale     => "Grayscale"
      case VideoEffect.Sepia         => "Sepia"
      case VideoEffect.Invert        => "Invert"
      case VideoEffect.Brightness(_) => "Brightness"
      case VideoEffect.ZoomIn(_)     => "ZoomIn"
      case VideoEffect.Shake(_, _)   => "Shake"
      case VideoEffect.FadeIn(_)     => "FadeIn"
    effectComboBox.value = name
    isUpdatingUi = false