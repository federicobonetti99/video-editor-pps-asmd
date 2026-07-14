package app.view.components

import scalafx.scene.layout.Pane
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.paint.Color
import scalafx.scene.control.Label
import scalafx.application.Platform
import core.model.Timeline
import java.io.File

class TimelinePanel(pixelsPerSecond: Double = 20.0, trackHeight: Double = 60.0) extends Pane:

  minHeight = 150
  prefWidth = 600
  style = "-fx-background-color: #2c3e50; -fx-border-color: #7f8c8d; -fx-border-width: 2;"

  private val playheadLine = new Line {
    startY = 0
    endY = 150
    stroke = Color.Red
    strokeWidth = 2
  }

  children = Seq(playheadLine)

  def updatePlayhead(seconds: Double): Unit =
    playheadLine.startX = seconds * pixelsPerSecond
    playheadLine.endX = seconds * pixelsPerSecond

  def draw(timeline: Timeline, currentCursorTime: Double): Unit =
    Platform.runLater {
      children.clear()
      children.add(playheadLine)

      timeline.videoTracks.foreach { track =>
        track.clips.foreach { videoClip =>
          val clipRectangle = new Rectangle {
            x = videoClip.startTime * pixelsPerSecond
            y = 45
            width = videoClip.duration * pixelsPerSecond
            height = trackHeight
            fill = Color.DeepSkyBlue
            stroke = Color.White
            strokeWidth = 2
            arcWidth = 8
            arcHeight = 8
          }

          val clipLabel = new Label {
            text = new File(videoClip.sourceUrl).getName
            layoutX = (videoClip.startTime * pixelsPerSecond) + 5
            layoutY = 65
            style = "-fx-text-fill: black; -fx-font-weight: bold; -fx-font-size: 11px;"
            maxWidth = (videoClip.duration * pixelsPerSecond) - 10
          }

          children.addAll(clipRectangle, clipLabel)
        }
      }
      updatePlayhead(currentCursorTime)
    }