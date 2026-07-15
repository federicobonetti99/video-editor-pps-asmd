package app.view.components

import scalafx.scene.layout.HBox
import scalafx.scene.control.{Button, Label}
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

  val btnImport = new Button("📥 Import Video File") { onAction = _ => onImport() }
  val btnDelete = new Button("🗑️ Delete Selected") { onAction = _ => onDelete() }
  val btnCut = new Button("Cut at Cursor") { onAction = _ => onCut() }
  val btnSnap = new Button("Snap Clips") { onAction = _ => onSnap() }
  val btnPlay = new Button("Play/Pause") { onAction = _ => onPlay() }

  val timeLabel = new Label {
    text = "Time: 00:00.00"
    style = "-fx-text-fill: white; -fx-font-family: 'Courier New'; -fx-font-size: 14px;"
  }

  children = Seq(btnImport, btnDelete, btnCut, btnSnap, btnPlay, timeLabel)

  def updateTimeLabel(seconds: Double): Unit =
    timeLabel.text = f"Time: ${seconds.toInt / 60}%02d:${seconds % 60}%05.2f"