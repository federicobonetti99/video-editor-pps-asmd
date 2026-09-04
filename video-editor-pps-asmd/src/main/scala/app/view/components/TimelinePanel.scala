package app.view.components

import scalafx.Includes.*
import scalafx.scene.layout.Pane
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.control.Label
import scalafx.scene.Node
import scalafx.application.Platform
import scalafx.scene.Cursor
import core.model.*
import scalafx.scene.paint.Color

import java.io.File
import java.util.concurrent.atomic.AtomicReference

enum SelectedClip:
  case SelectedVideo(trackId: Int, clip: VisualClip)
  case SelectedAudio(trackId: Int, clip: AudioClip)

class TimelinePanel(
                     private var pixelsPerSecond: Double = 30.0,
                     trackHeight: Double = 50.0,
                     val onVideoClipClicked: (Int, VisualClip) => Unit = (_, _) => (),
                     val onAudioClipClicked: (Int, AudioClip) => Unit = (_, _) => (),
                     val onVideoClipMoved: (VisualClip, Int, Double) => Unit = (_, _, _) => (),
                     val onAudioClipMoved: (AudioClip, Int, Double) => Unit = (_, _, _) => (),
                     val onSeekRequested: Double => Unit = _ => ()
                   ) extends Pane:

  private val trackSpacing = 8.0
  private val headerMargin = 15.0
  private val trackLabelWidth = 50.0

  style = "-fx-background-color: #1e1e1e;"

  private val playheadLine = new Line:
    startY = 0
    endY = 250
    stroke = Color.Red
    strokeWidth = 2

  private def timeAtX(x: Double): Double =
    Math.max(0.0, (x - trackLabelWidth) / pixelsPerSecond)

  onMousePressed = event =>
    if event.x >= trackLabelWidth then
      onSeekRequested(timeAtX(event.x))

  onMouseDragged = event =>
    if event.x >= trackLabelWidth then
      onSeekRequested(timeAtX(event.x))

  def updateZoom(newPps: Double): Unit =
    pixelsPerSecond = newPps

  def getPixelsPerSecond: Double = pixelsPerSecond

  def updatePlayhead(seconds: Double): Unit =
    val xPos = trackLabelWidth + (seconds * pixelsPerSecond)
    playheadLine.startX = xPos
    playheadLine.endX = xPos

  private def renderTrackBackground(trackName: String, yPos: Double, totalWidth: Double, isAudio: Boolean): Seq[Node] =
    val trackBg = new Rectangle:
      x = 0
      y = yPos
      width = totalWidth
      height = trackHeight
      fill = if isAudio then Color.web("#252822") else Color.web("#22262b")
      stroke = Color.web("#333333")
      strokeWidth = 1.0

    val trackLabelBg = new Rectangle:
      x = 0
      y = yPos
      width = trackLabelWidth
      height = trackHeight
      fill = if isAudio then Color.web("#2d4a2d") else Color.web("#243b55")

    val trackLabel = new Label:
      text = trackName
      layoutX = 12
      layoutY = yPos + (trackHeight / 2) - 8
      style = "-fx-text-fill: white; -fx-font-weight: bold; -fx-font-size: 11px;"

    Seq(trackBg, trackLabelBg, trackLabel)

  private def renderClipNodes[C <: MediaClip](
                                               mediaClip: C,
                                               sourceTrackId: Int,
                                               yPos: Double,
                                               fillColor: Color,
                                               isSelected: Boolean,
                                               tracksMeta: List[(Int, Double)],
                                               onClick: (Int, C) => Unit,
                                               onMove: (C, Int, Double) => Unit
                                             ): Seq[Node] =
    val initialX = trackLabelWidth + (mediaClip.startTime * pixelsPerSecond)
    val dragStartX = new AtomicReference[Double](0.0)
    val dragStartY = new AtomicReference[Double](0.0)
    val hasDragged = new AtomicReference[Boolean](false)

    val clipRectangle = new Rectangle:
      x = initialX
      y = yPos
      width = mediaClip.duration * pixelsPerSecond
      height = trackHeight
      fill = fillColor
      stroke = if isSelected then Color.Yellow else Color.White
      strokeWidth = if isSelected then 3.0 else 1.5
      arcWidth = 6
      arcHeight = 6
      cursor = Cursor.Hand

    val clipLabel = new Label:
      text = new File(mediaClip.sourceUrl).getName
      layoutX = initialX + 5
      layoutY = yPos + 14
      style = "-fx-text-fill: black; -fx-font-weight: bold; -fx-font-size: 11px;"
      maxWidth = (mediaClip.duration * pixelsPerSecond) - 10
      cursor = Cursor.Hand

    val handleMousePressed: scalafx.scene.input.MouseEvent => Unit = event =>
      dragStartX.set(event.sceneX)
      dragStartY.set(event.sceneY)
      hasDragged.set(false)
      event.consume()

    val handleMouseDragged: scalafx.scene.input.MouseEvent => Unit = event =>
      val deltaX = event.sceneX - dragStartX.get()
      val deltaY = event.sceneY - dragStartY.get()
      if Math.abs(deltaX) > 2.0 || Math.abs(deltaY) > 2.0 then
        hasDragged.set(true)
        val clampedDeltaX = Math.max(-(initialX - trackLabelWidth), deltaX)
        clipRectangle.translateX = clampedDeltaX
        clipLabel.translateX = clampedDeltaX
        clipRectangle.translateY = deltaY
        clipLabel.translateY = deltaY
      event.consume()

    val handleMouseReleased: scalafx.scene.input.MouseEvent => Unit = event =>
      if hasDragged.get() then
        val finalX = Math.max(trackLabelWidth, initialX + clipRectangle.translateX.value)
        val newStartTime = (finalX - trackLabelWidth) / pixelsPerSecond
        val currentLocalCenterY = yPos + clipRectangle.translateY.value + (trackHeight / 2.0)
        val targetTrackId = tracksMeta
          .minByOption(t => Math.abs(currentLocalCenterY - (t._2 + trackHeight / 2.0)))
          .map(_._1)
          .getOrElse(sourceTrackId)

        onMove(mediaClip, targetTrackId, newStartTime)
      else
        onClick(sourceTrackId, mediaClip)
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
      val maxDuration = {
        val maxV = timeline.videoTracks.flatMap(_.clips).map(_.endTime).maxOption.getOrElse(0.0)
        val maxA = timeline.audioTracks.flatMap(_.clips).map(_.endTime).maxOption.getOrElse(0.0)
        Math.max(maxV, maxA)
      }

      val calculatedWidth = Math.max(2000.0, trackLabelWidth + (maxDuration * pixelsPerSecond) + 300.0)
      prefWidth = calculatedWidth

      val orderedVideoTracks = timeline.videoTracks.sortBy(_.id).reverse
      val orderedAudioTracks = timeline.audioTracks.sortBy(_.id)

      val baseVideoY = headerMargin
      val videoTracksMeta = orderedVideoTracks.zipWithIndex.map: (track, index) =>
        (track.id, baseVideoY + index * (trackHeight + trackSpacing))

      val videoBgNodes = orderedVideoTracks.zip(videoTracksMeta).flatMap: (track, meta) =>
        renderTrackBackground(s"V${track.id}", meta._2, calculatedWidth, isAudio = false)

      val videoClipNodes = orderedVideoTracks.zip(videoTracksMeta).flatMap: (track, meta) =>
        track.clips.flatMap: videoClip =>
          val isSelected = selectedClip.exists:
            case SelectedClip.SelectedVideo(tid, v) => tid == track.id && videoClip.isSameAs(v)
            case _                                  => false
          renderClipNodes(
            videoClip,
            track.id,
            meta._2,
            Color.DeepSkyBlue,
            isSelected,
            videoTracksMeta,
            onVideoClipClicked,
            onVideoClipMoved
          )

      val videoTotalHeight = baseVideoY + (orderedVideoTracks.size * (trackHeight + trackSpacing))
      val separatorYPos = videoTotalHeight + 4.0

      val separatorLine = new Line:
        startX = 0
        startY = separatorYPos
        endX = calculatedWidth
        endY = separatorYPos
        stroke = Color.web("#555555")
        strokeWidth = 2
        strokeDashArray.addAll(6.0, 4.0)

      val baseAudioY = separatorYPos + 12.0
      val audioTracksMeta = orderedAudioTracks.zipWithIndex.map: (track, index) =>
        (track.id, baseAudioY + index * (trackHeight + trackSpacing))

      val audioBgNodes = orderedAudioTracks.zip(audioTracksMeta).flatMap: (track, meta) =>
        renderTrackBackground(s"A${track.id}", meta._2, calculatedWidth, isAudio = true)

      val audioClipNodes = orderedAudioTracks.zip(audioTracksMeta).flatMap: (track, meta) =>
        track.clips.flatMap: audioClip =>
          val isSelected = selectedClip.exists:
            case SelectedClip.SelectedAudio(tid, a) => tid == track.id && audioClip.isSameAs(a)
            case _                                  => false
          renderClipNodes(
            audioClip,
            track.id,
            meta._2,
            Color.LightGreen,
            isSelected,
            audioTracksMeta,
            onAudioClipClicked,
            onAudioClipMoved
          )

      val totalHeight = baseAudioY + (orderedAudioTracks.size * (trackHeight + trackSpacing)) + 30.0
      prefHeight = Math.max(220.0, totalHeight)

      playheadLine.endY = prefHeight.value

      children = Seq(playheadLine) ++ videoBgNodes ++ videoClipNodes ++ Seq(separatorLine) ++ audioBgNodes ++ audioClipNodes
      updatePlayhead(currentCursorTime)