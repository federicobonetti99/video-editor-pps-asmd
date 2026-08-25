package app.view.components

import scalafx.Includes.*
import scalafx.geometry.Pos
import scalafx.scene.control.{Button, ComboBox, Label}
import scalafx.scene.layout.HBox
import core.model.VideoEffect

class ToolbarControls(
                       val onImport: () => Unit = () => (),
                       val onDelete: () => Unit = () => (),
                       val onCut: () => Unit = () => (),
                       val onSnap: () => Unit = () => (),
                       val onAddVideoTrack: () => Unit = () => (),
                       val onAddAudioTrack: () => Unit = () => (),
                       val onPlay: () => Unit = () => (),
                       val onExport: () => Unit = () => (),
                       val onEffectSelected: VideoEffect => Unit = _ => ()
                     ) extends HBox:

  spacing = 10
  alignment = Pos.CenterLeft
  style = "-fx-padding: 8; -fx-background-color: #2b2b2b; -fx-background-radius: 4;"

  val importButton: Button = new Button("Import"):
    style = "-fx-background-color: #4CAF50; -fx-text-fill: white; -fx-font-weight: bold;"
    onAction = _ => onImport()

  val exportButton: Button = new Button("Export"):
    style = "-fx-background-color: #9C27B0; -fx-text-fill: white; -fx-font-weight: bold;"
    onAction = _ => onExport()

  val deleteButton: Button = new Button("Delete"):
    style = "-fx-background-color: #f44336; -fx-text-fill: white;"
    onAction = _ => onDelete()

  val cutButton: Button = new Button("Cut"):
    style = "-fx-background-color: #2196F3; -fx-text-fill: white;"
    onAction = _ => onCut()

  val snapButton: Button = new Button("Snap"):
    style = "-fx-background-color: #ff9800; -fx-text-fill: white;"
    onAction = _ => onSnap()

  val addVideoTrackButton: Button = new Button("+ Video"):
    style = "-fx-background-color: #3f51b5; -fx-text-fill: white;"
    onAction = _ => onAddVideoTrack()

  val addAudioTrackButton: Button = new Button("+ Audio"):
    style = "-fx-background-color: #009688; -fx-text-fill: white;"
    onAction = _ => onAddAudioTrack()

  val playButton: Button = new Button("Play/Pause"):
    style = "-fx-background-color: #607d8b; -fx-text-fill: white;"
    onAction = _ => onPlay()

  val timeLabel: Label = new Label("Time: 0.0s"):
    style = "-fx-text-fill: white; -fx-font-family: monospace;"

  private val effectLabel = new Label("Effect:"):
    style = "-fx-text-fill: white;"
    visible = false
    managed = false

  private val effectComboBox = new ComboBox[String](
    Seq("None", "Grayscale", "Sepia", "Invert", "ZoomIn", "FadeIn", "Shake")
  ):
    value = "None"
    visible = false
    managed = false
    onAction = _ =>
      val selectedEffect = value.value match
        case "Grayscale" => VideoEffect.Grayscale
        case "Sepia"     => VideoEffect.Sepia
        case "Invert"    => VideoEffect.Invert
        case "ZoomIn"    => VideoEffect.ZoomIn(2.0)
        case "FadeIn"    => VideoEffect.FadeIn(2.0)
        case "Shake"     => VideoEffect.Shake(10.0, 5.0)
        case _           => VideoEffect.None
      onEffectSelected(selectedEffect)

  children = Seq(
    importButton,
    exportButton,
    deleteButton,
    cutButton,
    snapButton,
    addVideoTrackButton,
    addAudioTrackButton,
    playButton,
    timeLabel,
    effectLabel,
    effectComboBox
  )

  def updateTimeLabel(seconds: Double): Unit =
    timeLabel.text = f"Time: $seconds%.1fs"

  def setEffectControlsVisible(visible: Boolean): Unit =
    effectLabel.visible = visible
    effectLabel.managed = visible
    effectComboBox.visible = visible
    effectComboBox.managed = visible

  def setSelectedEffect(effect: VideoEffect): Unit =
    val stringVal = effect match
      case VideoEffect.None        => "None"
      case VideoEffect.Grayscale   => "Grayscale"
      case VideoEffect.Sepia       => "Sepia"
      case VideoEffect.Invert      => "Invert"
      case VideoEffect.ZoomIn(_)   => "ZoomIn"
      case VideoEffect.FadeIn(_)   => "FadeIn"
      case VideoEffect.Shake(_, _) => "Shake"
    effectComboBox.value = stringVal