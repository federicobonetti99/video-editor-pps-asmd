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

class TimelinePanel(
                     pixelsPerSecond: Double = 20.0,
                     trackHeight: Double = 50.0,
                     val onVideoClipClicked: VideoClip => Unit = _ => (),
                     val onAudioClipClicked: AudioClip => Unit = _ => ()
                   ) extends Pane:

  minHeight = 200
  prefWidth = 600
  style = "-fx-background-color: #2c3e50; -fx-border-color: #7f8c8d; -fx-border-width: 2;"

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
      val baseVideoY = 20.0
      val trackSpacing = 10.0

      val videoNodes = timeline.videoTracks.zipWithIndex.flatMap: (track, trackIndex) =>
        val trackY = baseVideoY + trackIndex * (trackHeight + trackSpacing)
        track.clips.flatMap: videoClip =>
          val isSelected = selectedClip.exists:
            case SelectedClip.SelectedVideo(v) => videoClip.isSameAs(v)
            case _                             => false
          renderClipNodes(videoClip, trackY, Color.DeepSkyBlue, isSelected, onVideoClipClicked)

      val videoSectionHeight = baseVideoY + timeline.videoTracks.size * (trackHeight + trackSpacing)
      val separatorYPos = videoSectionHeight + 5.0
      val separatorLine = new Line:
        startX = 0
        startY = separatorYPos
        endX = 2000
        endY = separatorYPos
        stroke = Color.web("#7f8c8d")
        strokeWidth = 1
        strokeDashArray.addAll(5.0, 5.0)

      val baseAudioY = separatorYPos + 15.0
      val audioNodes = timeline.audioTracks.zipWithIndex.flatMap: (track, trackIndex) =>
        val trackY = baseAudioY + trackIndex * (trackHeight + trackSpacing)
        track.clips.flatMap: audioClip =>
          val isSelected = selectedClip.exists:
            case SelectedClip.SelectedAudio(a) => audioClip.isSameAs(a)
            case _                             => false
          renderClipNodes(audioClip, trackY, Color.LightGreen, isSelected, onAudioClipClicked)

      children = Seq(playheadLine) ++ videoNodes ++ Seq(separatorLine) ++ audioNodes
      updatePlayhead(currentCursorTime)