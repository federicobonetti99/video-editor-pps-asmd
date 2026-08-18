package app.view.components

import scalafx.Includes.*
import scalafx.scene.layout.Pane
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.control.Label
import scalafx.scene.Node
import scalafx.application.Platform
import scalafx.scene.Cursor
import core.model.*
import java.io.File
import java.util.concurrent.atomic.AtomicReference

enum SelectedClip:
  case SelectedVideo(clip: VideoClip)
  case SelectedAudio(clip: AudioClip)

class TimelinePanel(
                     pixelsPerSecond: Double = 20.0,
                     trackHeight: Double = 50.0,
                     val onVideoClipClicked: VideoClip => Unit = _ => (),
                     val onAudioClipClicked: AudioClip => Unit = _ => (),
                     val onVideoClipMoved: (VideoClip, Double) => Unit = (_, _) => (),
                     val onAudioClipMoved: (AudioClip, Double) => Unit = (_, _) => ()
                   ) extends Pane:

  prefHeight = 250
  prefWidth = 2000
  style = "-fx-background-color: #222222;"

  private val pixelsPerSecond = 20.0
  private val trackHeight = 50.0

  var onClipSelected: (String, Int, Int) => Unit = (_, _, _) => ()

  private val playheadLine = new Line:
    startY = 0
    endY = 200
    stroke = Color.Red
    strokeWidth = 2

  def updatePlayhead(seconds: Double): Unit =
    val xPos = seconds * pixelsPerSecond
    playheadLine.startX = xPos
    playheadLine.endX = xPos

  private def renderClipNodes[C <: MediaClip](
                                               mediaClip: C,
                                               yPos: Double,
                                               fillColor: Color,
                                               isSelected: Boolean,
                                               onClick: C => Unit,
                                               onMove: (C, Double) => Unit
                                             ): Seq[Node] =
    val initialX = mediaClip.startTime * pixelsPerSecond
    val dragStartX = new AtomicReference[Double](0.0)
    val hasDragged = new AtomicReference[Boolean](false)

    val clipRectangle = new Rectangle:
      x = initialX
      y = yPos
      width = mediaClip.duration * pixelsPerSecond
      height = trackHeight
      fill = fillColor
      stroke = if isSelected then Color.Yellow else Color.White
      strokeWidth = if isSelected then 4.0 else 2.0
      arcWidth = 8
      arcHeight = 8
      cursor = Cursor.Hand

    val clipLabel = new Label:
      text = new File(mediaClip.sourceUrl).getName
      layoutX = initialX + 5
      layoutY = yPos + 12
      style = "-fx-text-fill: black; -fx-font-weight: bold; -fx-font-size: 11px;"
      maxWidth = (mediaClip.duration * pixelsPerSecond) - 10
      cursor = Cursor.Hand

    val handleMousePressed: scalafx.scene.input.MouseEvent => Unit = event =>
      dragStartX.set(event.sceneX)
      hasDragged.set(false)
      event.consume()

    val handleMouseDragged: scalafx.scene.input.MouseEvent => Unit = event =>
      val deltaX = event.sceneX - dragStartX.get()
      if Math.abs(deltaX) > 2.0 then
        hasDragged.set(true)
        val clampedDeltaX = Math.max(-initialX, deltaX)
        clipRectangle.translateX = clampedDeltaX
        clipLabel.translateX = clampedDeltaX
      event.consume()

    val handleMouseReleased: scalafx.scene.input.MouseEvent => Unit = event =>
      if hasDragged.get() then
        val finalX = Math.max(0.0, initialX + clipRectangle.translateX.value)
        val newStartTime = finalX / pixelsPerSecond
        onMove(mediaClip, newStartTime)
      else
        onClick(mediaClip)
      event.consume()

    clipRectangle.onMousePressed = handleMousePressed
    clipRectangle.onMouseDragged = handleMouseDragged
    clipRectangle.onMouseReleased = handleMouseReleased

    clipLabel.onMousePressed = handleMousePressed
    clipLabel.onMouseDragged = handleMouseDragged
    clipLabel.onMouseReleased = handleMouseReleased

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
          renderClipNodes(videoClip, trackY, Color.DeepSkyBlue, isSelected, onVideoClipClicked, onVideoClipMoved)

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
          renderClipNodes(audioClip, trackY, Color.LightGreen, isSelected, onAudioClipClicked, onAudioClipMoved)

      children = Seq(playheadLine) ++ videoNodes ++ Seq(separatorLine) ++ audioNodes
      updatePlayhead(currentCursorTime)