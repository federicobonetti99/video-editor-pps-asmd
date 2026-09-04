package app.view

import scalafx.Includes.*
import scalafx.application.Platform
import scalafx.geometry.Pos
import scalafx.scene.control.{Label, ScrollPane, Slider}
import scalafx.scene.layout.{HBox, Priority, Region, StackPane, VBox}
import scalafx.beans.property.ObjectProperty
import core.model.*
import app.view.components.*
import view.EffectInspectorPane

class TimelineView(
                    val onDeleteRequested: () => Unit = () => (),
                    val onCutRequested: Double => Unit = _ => (),
                    val onSnapRequested: () => Unit = () => (),
                    val onTogglePlaybackRequested: () => Unit = () => (),
                    val onTimeChanged: Double => Unit = _ => (),
                    val onImportRequested: () => Unit = () => (),
                    val onExportRequested: () => Unit = () => (),
                    val onUndoRequested: () => Unit = () => (),
                    val onRedoRequested: () => Unit = () => (),
                    val onVideoTimeUpdated: Double => Unit = _ => (),
                    val onClipSelected: Option[SelectedClip] => Unit = _ => (),
                    val onClipMoved: (MediaClip, Int, Double) => Unit = (_, _, _) => (),
                    val onEffectSelected: VideoEffect => Unit = _ => (),
                    val onAudioVolumeChanged: Double => Unit = _ => (),
                    val onAddVideoTrackRequested: () => Unit = () => (),
                    val onAddAudioTrackRequested: () => Unit = () => ()
                  ) extends VBox:

  spacing = 0
  style = "-fx-padding: 0; -fx-background-color: #1a1a1a;"

  private val selectedClipProperty = ObjectProperty[Option[SelectedClip]](None)
  private val currentTimelineProperty = ObjectProperty[Option[Timeline]](None)
  private val currentTrackedTimeProperty = ObjectProperty[Double](0.0)

  private val preview = new VideoPreview(480.0, 270.0)
  private val audioPlayer = new AudioPlayer()

  private val topMask = new Region:
    prefHeight = 20.0
    minHeight = 20.0
    maxHeight = 20.0
    style = "-fx-background-color: #1e1e1e; -fx-border-color: #2d2d2d; -fx-border-width: 0 0 1px 0;"

  private val leftMask = new Region:
    hgrow = Priority.Always
    minWidth = 0.0
    prefHeight = 270.0
    minHeight = 270.0
    maxHeight = 270.0
    style = "-fx-background-color: #1e1e1e; -fx-border-color: #2d2d2d; -fx-border-width: 0 1px 0 0;"

  private val middleSpacer = new Region:
    minWidth = 480.0
    prefWidth = 480.0
    maxWidth = 480.0
    prefHeight = 270.0
    minHeight = 270.0
    maxHeight = 270.0
    mouseTransparent = true

  private val rightMaskBackground = new Region:
    prefHeight = 270.0
    minHeight = 270.0
    maxHeight = 270.0
    style = "-fx-background-color: #1e1e1e; -fx-border-color: #2d2d2d; -fx-border-width: 0 0 0 1px;"

  private val effectInspector = new EffectInspectorPane(
    onEffectChanged = effect => onEffectSelected(effect),
    onAudioVolumeChanged = volume => onAudioVolumeChanged(volume)
  )

  private val rightMask = new StackPane:
    hgrow = Priority.Always
    minWidth = 0.0
    prefHeight = 270.0
    minHeight = 270.0
    maxHeight = 270.0
    alignment = Pos.TopLeft
    children = Seq(rightMaskBackground, effectInspector)

  private val sideMasksRow = new HBox:
    alignment = Pos.Center
    pickOnBounds = false
    prefHeight = 270.0
    minHeight = 270.0
    maxHeight = 270.0
    children = Seq(leftMask, middleSpacer, rightMask)

  HBox.setHgrow(leftMask, Priority.Always)
  HBox.setHgrow(rightMask, Priority.Always)
  StackPane.setAlignment(effectInspector, Pos.TopLeft)

  private val zoomLabel = new Label("Zoom:"):
    style = "-fx-text-fill: #aaaaaa; -fx-font-size: 11px;"

  private val zoomSlider = new Slider:
    min = 5.0
    max = 100.0
    value = 30.0
    prefWidth = 150
    maxWidth = 200
    focusTraversable = false

  private val zoomControls = new HBox:
    spacing = 8
    alignment = Pos.CenterRight
    style = "-fx-padding: 0 15 0 0;"
    children = Seq(zoomLabel, zoomSlider)

  private val bottomMaskBackground = new Region:
    prefHeight = 55.0
    minHeight = 55.0
    maxHeight = 55.0
    style = "-fx-background-color: #1e1e1e; -fx-border-color: #2d2d2d; -fx-border-width: 1px 0 0 0;"

  private val bottomMaskBar = new StackPane:
    prefHeight = 55.0
    minHeight = 55.0
    maxHeight = 55.0
    children = Seq(bottomMaskBackground, zoomControls)

  private val masksOverlay = new VBox:
    spacing = 0
    alignment = Pos.Center
    pickOnBounds = false
    children = Seq(topMask, sideMasksRow, bottomMaskBar)

  private val previewViewport = new StackPane:
    alignment = Pos.Center
    prefHeight = 345.0
    minHeight = 345.0
    maxHeight = 345.0
    children = Seq(preview, masksOverlay)

  private val timelinePanel: TimelinePanel = new TimelinePanel(
    pixelsPerSecond = 30.0,
    onVideoClipClicked = (trackId, clip) => toggleVideoSelection(trackId, clip),
    onAudioClipClicked = (trackId, clip) => toggleAudioSelection(trackId, clip),
    onVideoClipMoved = (clip, targetTrackId, newTime) => onClipMoved(clip, targetTrackId, newTime),
    onAudioClipMoved = (clip, targetTrackId, newTime) => onClipMoved(clip, targetTrackId, newTime),
    onSeekRequested = newTime => handleUserSeek(newTime)
  )

  zoomSlider.valueProperty.addListener: (_, _, newValue) =>
    val newPps = newValue.doubleValue()
    timelinePanel.updateZoom(newPps)
    currentTimelineProperty.value.foreach(render)

  private val timelineScrollPane = new ScrollPane:
    content = timelinePanel
    prefHeight = 220
    minHeight = 150
    fitToWidth = false
    fitToHeight = false
    hbarPolicy = ScrollPane.ScrollBarPolicy.AsNeeded
    vbarPolicy = ScrollPane.ScrollBarPolicy.AsNeeded
    style = "-fx-background: #1e1e1e; -fx-border-color: #333333; -fx-border-width: 1px;"

  private val toolbar = new ToolbarControls(
    onImport = () => onImportRequested(),
    onExport = () => onExportRequested(),
    onUndo   = () => onUndoRequested(),
    onRedo   = () => onRedoRequested(),
    onDelete = () => onDeleteRequested(),
    onCut    = () => onCutRequested(currentTrackedTimeProperty.value),
    onSnap   = () => onSnapRequested(),
    onAddVideoTrack = () => onAddVideoTrackRequested(),
    onAddAudioTrack = () => onAddAudioTrackRequested(),
    onPlay   = () => onTogglePlaybackRequested(),
    onEffectSelected = effect => onEffectSelected(effect)
  )

  private val bottomSection = new VBox:
    spacing = 15
    style = "-fx-padding: 0 15 15 15;"
    children = Seq(timelineScrollPane, toolbar)

  children = Seq(previewViewport, bottomSection)

  private def handleUserSeek(newTime: Double): Unit =
    currentTrackedTimeProperty.value = newTime
    toolbar.updateTimeLabel(newTime)
    timelinePanel.updatePlayhead(newTime)
    onTimeChanged(newTime)

  def getSelectedClip: Option[SelectedClip] = selectedClipProperty.value

  def updateHistoryControls(canUndo: Boolean, canRedo: Boolean): Unit =
    toolbar.updateHistoryButtons(canUndo, canRedo)

  def selectClip(targetOpt: Option[SelectedClip]): Unit =
    selectedClipProperty.value = targetOpt
    targetOpt match
      case Some(SelectedClip.SelectedVideo(_, v)) =>
        toolbar.setSelectedEffect(v.effect)
        toolbar.setEffectControlsVisible(true)
        effectInspector.updateSelection(Some(v))
      case Some(SelectedClip.SelectedAudio(_, a)) =>
        toolbar.setSelectedEffect(VideoEffect.None)
        toolbar.setEffectControlsVisible(false)
        effectInspector.updateSelection(Some(a))
      case _ =>
        toolbar.setSelectedEffect(VideoEffect.None)
        toolbar.setEffectControlsVisible(false)
        effectInspector.updateSelection(None)

    onClipSelected(targetOpt)
    currentTimelineProperty.value.foreach(render)

  def toggleVideoSelection(trackId: Int, clip: VideoClip): Unit =
    val isAlreadySelected = selectedClipProperty.value.exists:
      case SelectedClip.SelectedVideo(tid, v) => tid == trackId && clip.isSameAs(v)
      case _                                  => false

    if isAlreadySelected then selectClip(None)
    else selectClip(Some(SelectedClip.SelectedVideo(trackId, clip)))

  def toggleAudioSelection(trackId: Int, clip: AudioClip): Unit =
    val isAlreadySelected = selectedClipProperty.value.exists:
      case SelectedClip.SelectedAudio(tid, a) => tid == trackId && clip.isSameAs(a)
      case _                                  => false

    if isAlreadySelected then selectClip(None)
    else selectClip(Some(SelectedClip.SelectedAudio(trackId, clip)))

  def updateTimelineTime(seconds: Double): Unit =
    currentTrackedTimeProperty.value = seconds
    Platform.runLater:
      toolbar.updateTimeLabel(seconds)
      timelinePanel.updatePlayhead(seconds)

  def updatePreview(
                     videoUrlOpt: Option[String],
                     relativeTimeSeconds: Double,
                     isPlaying: Boolean,
                     effect: VideoEffect = VideoEffect.None,
                     clipDuration: Double = 0.0
                   ): Unit =
    preview.update(
      PlaybackState(
        mediaUrl = videoUrlOpt,
        relativeTime = relativeTimeSeconds,
        isPlaying = isPlaying,
        effect = effect,
        clipDuration = clipDuration
      ),
      onVideoTimeUpdated
    )

  def updateAudio(activeAudios: List[ActiveAudioTrackInfo], isPlaying: Boolean): Unit =
    audioPlayer.update(activeAudios, isPlaying)

  def render(timeline: Timeline): Unit =
    currentTimelineProperty.value = Some(timeline)
    timelinePanel.draw(timeline, currentTrackedTimeProperty.value, selectedClipProperty.value)