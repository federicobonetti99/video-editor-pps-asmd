package app.view.components

import scalafx.Includes.*
import scalafx.scene.layout.Pane
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.control.Label
import scalafx.scene.paint.Color
import scalafx.application.Platform
import core.model.*
import java.io.File

class TimelinePanel extends Pane:

  prefHeight = 250
  prefWidth = 2000
  style = "-fx-background-color: #222222;"

  private val pixelsPerSecond = 20.0
  private val trackHeight = 50.0

  var onClipSelected: (String, Int, Int) => Unit = (_, _, _) => ()

  private val playheadLine = new Line {
    startX = 0; startY = 0; endX = 0; endY = 250
    stroke = Color.Red; strokeWidth = 2
  }

  def updatePlayhead(seconds: Double): Unit =
    val xPos = seconds * pixelsPerSecond
    playheadLine.startX = xPos
    playheadLine.endX = xPos

  def draw(timeline: Timeline, currentCursorTime: Double, selectedClip: Option[(String, Int, Int)] = None): Unit =
    Platform.runLater {
      children.clear()
      children.add(playheadLine)

      var currentY = 20.0
      val trackSpacing = 10.0

      timeline.videoTracks.foreach { track =>
        track.clips.zipWithIndex.foreach { case (videoClip, index) =>

          val isSelected = selectedClip.contains(("video", track.id, index))

          val clipRectangle = new Rectangle {
            x = videoClip.startTime * pixelsPerSecond
            y = currentY
            width = videoClip.duration * pixelsPerSecond
            height = trackHeight

            fill = if isSelected then Color.web("#00bcd4") else Color.DeepSkyBlue
            stroke = if isSelected then Color.Gold else Color.White
            strokeWidth = if isSelected then 4 else 2
            arcWidth = 8; arcHeight = 8

            onMouseClicked = _ => onClipSelected("video", track.id, index)
          }

          val clipLabel = new Label {
            text = new File(videoClip.sourceUrl).getName
            layoutX = (videoClip.startTime * pixelsPerSecond) + 5
            layoutY = currentY + 15
            style = "-fx-text-fill: black; -fx-font-weight: bold; -fx-font-size: 11px;"
            maxWidth = (videoClip.duration * pixelsPerSecond) - 10
            // Permettiamo il click anche sopra l'etichetta di testo
            onMouseClicked = _ => onClipSelected("video", track.id, index)
          }

          children.addAll(clipRectangle, clipLabel)
        }
        currentY += trackHeight + trackSpacing
      }

      val separatorY = currentY + 5.0
      val separatorLine = new Line {
        startX = 0; startY = separatorY; endX = 2000; endY = separatorY
        stroke = Color.web("#7f8c8d"); strokeWidth = 1
        strokeDashArray.addAll(5.0, 5.0)
      }
      children.add(separatorLine)
      currentY = separatorY + 15.0

      timeline.audioTracks.foreach { track =>
        track.clips.zipWithIndex.foreach { case (audioClip, index) =>

          val isSelected = selectedClip.contains(("audio", track.id, index))

          val clipRectangle = new Rectangle {
            x = audioClip.startTime * pixelsPerSecond
            y = currentY
            width = audioClip.duration * pixelsPerSecond
            height = trackHeight

            fill = if isSelected then Color.web("#8bc34a") else Color.LightGreen
            stroke = if isSelected then Color.Gold else Color.White
            strokeWidth = if isSelected then 4 else 2
            arcWidth = 8; arcHeight = 8

            onMouseClicked = _ => onClipSelected("audio", track.id, index)
          }

          val clipLabel = new Label {
            text = new File(audioClip.sourceUrl).getName
            layoutX = (audioClip.startTime * pixelsPerSecond) + 5
            layoutY = currentY + 15
            style = "-fx-text-fill: black; -fx-font-weight: bold; -fx-font-size: 11px;"
            maxWidth = (audioClip.duration * pixelsPerSecond) - 10
            onMouseClicked = _ => onClipSelected("audio", track.id, index)
          }

          children.addAll(clipRectangle, clipLabel)
        }
        currentY += trackHeight + trackSpacing
      }

      updatePlayhead(currentCursorTime)
    }