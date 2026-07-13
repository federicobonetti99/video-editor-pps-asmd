package app.view

import scalafx.application.Platform
import scalafx.scene.control.{Button, Label, Slider}
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.paint.Color
import core.model.*
import scalafx.scene.layout.{HBox, Pane, VBox}
import scalafx.geometry.Pos

case class ViewContext(pixelsPerSecond: Double = 20.0, trackHeight: Double = 60.0)

class TimelineView extends VBox:

  spacing = 15
  style = "-fx-padding: 10; -fx-background-color: #1a1a1a;"

  private val viewCtx = ViewContext()

  var onAddRequested: Double => Unit = _ => ()
  var onCutRequested: Double => Unit = _ => ()
  var onSnapRequested: () => Unit = () => ()

  private val timeSlider = new Slider {
    min = 0.0
    max = 100.0
    value = 0.0
    prefWidth = 600
  }

  private val timeLabel = new Label {
    text = "Time: 00:00.00"
    style = "-fx-text-fill: white; -fx-font-family: 'Courier New'; -fx-font-size: 14px;"
  }

  timeSlider.valueProperty.addListener { (_, _, newValue) =>
    val seconds = newValue.doubleValue()
    timeLabel.text = f"Time: ${seconds.toInt / 60}%02d:${seconds % 60}%05.2f"
    updatePlayheadPosition(seconds)
  }

  private val timelinePane = new Pane {
    minHeight = 150
    prefWidth = 600
    style = "-fx-background-color: #2c3e50; -fx-border-color: #7f8c8d; -fx-border-width: 2;"
  }

  private val playheadLine = new Line {
    startY = 0
    endY = 150
    stroke = Color.Red
    strokeWidth = 2
  }

  private val controls = new HBox {
    spacing = 10
    alignment = Pos.CenterLeft

    val btnAdd = new Button("Add Clip at Cursor")
    val btnCut = new Button("Cut at Cursor")
    val btnSnap = new Button("Snap Clips")

    btnAdd.onAction = _ => onAddRequested(timeSlider.value.value)
    btnCut.onAction = _ => onCutRequested(timeSlider.value.value)
    btnSnap.onAction = _ => onSnapRequested()

    children = Seq(btnAdd, btnCut, btnSnap, timeLabel)
  }

  children = Seq(timeSlider, timelinePane, controls)

  private def updatePlayheadPosition(seconds: Double): Unit =
    playheadLine.startX = seconds * viewCtx.pixelsPerSecond
    playheadLine.endX = seconds * viewCtx.pixelsPerSecond

  def render(timeline: Timeline): Unit =
    Platform.runLater {
      timelinePane.children.clear()
      timelinePane.children.add(playheadLine)

      timeline.videoTracks.foreach { track =>
        track.clips.foreach { videoClip =>
          val clipRectangle = new Rectangle {
            x = videoClip.startTime * viewCtx.pixelsPerSecond
            y = 45
            width = videoClip.duration * viewCtx.pixelsPerSecond
            height = viewCtx.trackHeight
            fill = Color.DeepSkyBlue
            stroke = Color.White
            strokeWidth = 2
            arcWidth = 8
            arcHeight = 8
          }
          timelinePane.children.add(clipRectangle)
        }
      }
      updatePlayheadPosition(timeSlider.value.value)
    }