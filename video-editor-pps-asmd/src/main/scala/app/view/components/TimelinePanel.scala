package app.view.components

import scalafx.Includes.*
import scalafx.scene.layout.Pane
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.paint.Color
import scalafx.scene.control.Label
import scalafx.scene.Node
import scalafx.application.Platform
import core.model.*
import java.io.File

enum SelectedClip:
  case SelectedVideo(clip: VideoClip)
  case SelectedAudio(clip: AudioClip)

class TimelinePanel(pixelsPerSecond: Double = 20.0, trackHeight: Double = 50.0) extends Pane:

  minHeight = 200
  prefWidth = 600
  style = "-fx-background-color: #2c3e50; -fx-border-color: #7f8c8d; -fx-border-width: 2;"

  var onVideoClipClicked: VideoClip => Unit = _ => ()
  var onAudioClipClicked: AudioClip => Unit = _ => ()

  private var selectedClipOpt: Option[SelectedClip] = None

  private val playheadLine = new Line:
    startY = 0
    endY = 200
    stroke = Color.Red
    strokeWidth = 2

  children = Seq(playheadLine)

  def updatePlayhead(seconds: Double): Unit =
    playheadLine.startX = seconds * pixelsPerSecond
    playheadLine.endX = seconds * pixelsPerSecond

  private def renderClipNodes[C <: MediaClip](
                                               mediaClip: C,
                                               yPos: Double,
                                               fillColor: Color,
                                               isSelected: Boolean,
                                               onClick: C => Unit
                                             ): Seq[Node] =
    val clipRectangle = new Rectangle:
      x = mediaClip.startTime * pixelsPerSecond
      y = yPos
      width = mediaClip.duration * pixelsPerSecond
      height = trackHeight
      fill = fillColor
      stroke = if isSelected then Color.Yellow else Color.White
      strokeWidth = if isSelected then 4.0 else 2.0
      arcWidth = 8
      arcHeight = 8
      onMouseClicked = event =>
        onClick(mediaClip)
        event.consume()

    val clipLabel = new Label:
      text = new File(mediaClip.sourceUrl).getName
      layoutX = (mediaClip.startTime * pixelsPerSecond) + 5
      layoutY = yPos + 12
      style = "-fx-text-fill: black; -fx-font-weight: bold; -fx-font-size: 11px;"
      maxWidth = (mediaClip.duration * pixelsPerSecond) - 10
      onMouseClicked = event =>
        onClick(mediaClip)
        event.consume()

    Seq(clipRectangle, clipLabel)

  def draw(timeline: Timeline, currentCursorTime: Double, selectedClip: Option[SelectedClip] = None): Unit =
    Platform.runLater:
      children.clear()
      children.add(playheadLine)
      selectedClipOpt = selectedClip

      var currentY = 20.0
      val trackSpacing = 10.0

      timeline.videoTracks.foreach: track =>
        track.clips.foreach: videoClip =>
          val isSelected = selectedClipOpt.exists:
            case SelectedClip.SelectedVideo(v) => videoClip.isSameAs(v)
            case _                             => false

          val nodes = renderClipNodes(videoClip, currentY, Color.DeepSkyBlue, isSelected, onVideoClipClicked)
          nodes.foreach(children.add(_))
        currentY += trackHeight + trackSpacing

      val separatorY = currentY + 5.0
      val separatorLine = new Line:
        startX = 0
        startY = separatorY
        endX = 2000
        endY = separatorY
        stroke = Color.web("#7f8c8d")
        strokeWidth = 1
        strokeDashArray.addAll(5.0, 5.0)
      children.add(separatorLine)

      currentY = separatorY + 15.0

      timeline.audioTracks.foreach: track =>
        track.clips.foreach: audioClip =>
          val isSelected = selectedClipOpt.exists:
            case SelectedClip.SelectedAudio(a) => audioClip.isSameAs(a)
            case _                             => false

          val nodes = renderClipNodes(audioClip, currentY, Color.LightGreen, isSelected, onAudioClipClicked)
          nodes.foreach(children.add(_))
        currentY += trackHeight + trackSpacing

      updatePlayhead(currentCursorTime)