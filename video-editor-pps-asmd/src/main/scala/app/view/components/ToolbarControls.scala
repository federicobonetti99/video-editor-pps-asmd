package app.view.components

import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.HBox
import scalafx.geometry.Pos

class ToolbarControls(
                       onImport: () => Unit,
                       onDelete: () => Unit,
                       onCut: () => Unit,
                       onSnap: () => Unit,
                       onPlay: () => Unit
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

  children = Seq(importButton, deleteButton, cutButton, snapButton, playButton, timeLabel)

  def updateTimeLabel(seconds: Double): Unit =
    val totalSeconds = seconds.toInt
    val minutes = totalSeconds / 60
    val remSeconds = totalSeconds % 60
    val millis = ((seconds - totalSeconds) * 1000).toInt
    timeLabel.text = f"$minutes%02d:$remSeconds%02d.$millis%03d"