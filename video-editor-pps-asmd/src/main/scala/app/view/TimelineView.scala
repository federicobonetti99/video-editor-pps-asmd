package app.view

import scalafx.application.{JFXApp3, Platform}
import scalafx.scene.control.Button
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import core.model.*
import scalafx.scene.layout.{HBox, Pane, VBox}

class TimelineView(
                    onAddRequested: () => Unit,
                    onCutRequested: () => Unit,
                    onSnapRequested: () => Unit
                  ) extends VBox:

  spacing = 15
  style = "-fx-padding: 10;"

  private val timelinePane = new Pane {
    minHeight = 150
    prefWidth = 600
    style = "-fx-background-color: #2c3e50; -fx-border-color: #7f8c8d; -fx-border-width: 2;"
  }

  private val controls = new HBox {
    spacing = 10
    val btnAdd = new Button("Add Sample Clip")
    val btnCut = new Button("Cut First Clip (at 3s)")
    val btnSnap = new Button("Snap Clips")

    btnAdd.onAction = _ => onAddRequested()
    btnCut.onAction = _ => onCutRequested()
    btnSnap.onAction = _ => onSnapRequested()

    children = Seq(btnAdd, btnCut, btnSnap)
  }

  children = Seq(controls, timelinePane)

  def render(timeline: Timeline): Unit =
    Platform.runLater {
      timelinePane.children.clear()

      timeline.videoTracks.foreach { track =>
        track.clips.foreach { videoClip =>
          val clipRectangle = new Rectangle {
            x = videoClip.startTime * 20.0
            y = 45 
            width = videoClip.duration * 20.0
            height = 60
            fill = Color.DeepSkyBlue
            stroke = Color.White
            strokeWidth = 2
            arcWidth = 8
            arcHeight = 8
          }
          timelinePane.children.add(clipRectangle)
        }
      }
    }