package app.view

import scalafx.Includes.*
import scalafx.application.Platform
import scalafx.scene.control.{ScrollPane, Slider}
import scalafx.scene.layout.VBox
import scalafx.beans.property.ObjectProperty
import core.model.*
import app.view.components.*

class TimelineView(
                    val onDeleteRequested: () => Unit = () => (),
                    val onCutRequested: Double => Unit = _ => (),
                    val onSnapRequested: () => Unit = () => (),
                    val onTogglePlaybackRequested: () => Unit = () => (),
                    val onTimeChanged: Double => Unit = _ => (),
                    val onImportRequested: () => Unit = () => (),
                    val onVideoTimeUpdated: Double => Unit = _ => (),
                    val onClipSelected: Option[SelectedClip] => Unit = _ => (),
                    val onClipMoved: (MediaClip, Double) => Unit = (_, _) => (),
                    val onEffectSelected: VideoEffect => Unit = _ => ()
                  ) extends VBox:

  spacing = 15
  style = "-fx-padding: 15; -fx-background-color: #1a1a1a;"

  private val selectedClipProperty = ObjectProperty[Option[SelectedClip]](None)
  private val currentTimelineProperty = ObjectProperty[Option[Timeline]](None)

  private val preview = new VideoPreview(480.0, 270.0)
  private val audioPlayer = new AudioPlayer()

  private val timelinePanel = new TimelinePanel(
    onVideoClipClicked = clip => toggleVideoSelection(clip),
    onAudioClipClicked = clip => toggleAudioSelection(clip),
    onVideoClipMoved = (clip, newTime) => onClipMoved(clip, newTime),
    onAudioClipMoved = (clip, newTime) => onClipMoved(clip, newTime)
  )

  private val timelineScrollPane = new ScrollPane:
    content = timelinePanel
    prefHeight = 200
    minHeight = 150
    fitToWidth = false
    fitToHeight = false
    hbarPolicy = ScrollPane.ScrollBarPolicy.AsNeeded
    vbarPolicy = ScrollPane.ScrollBarPolicy.AsNeeded
    style = "-fx-background: #1e1e1e; -fx-border-color: #333333; -fx-border-width: 1px;"

  private val timeSlider = new Slider:
    min = 0.0
    max = 60.0
    value = 0.0
    prefWidth = 600
    maxWidth = 800
    focusTraversable = false

  private val toolbar = new ToolbarControls(
    onImport = () => onImportRequested(),
    onDelete = () => onDeleteRequested(),
    onCut    = () => onCutRequested(timeSlider.value.value),
    onSnap   = () => onSnapRequested(),
    onPlay   = () => onTogglePlaybackRequested(),
    onEffectSelected = effect => onEffectSelected(effect)
  )

  children = Seq(preview, timeSlider, timelineScrollPane, toolbar)

  timeSlider.valueProperty.addListener: (_, _, newValue) =>
    val seconds = newValue.doubleValue()
    toolbar.updateTimeLabel(seconds)
    timelinePanel.updatePlayhead(seconds)
    if timeSlider.isValueChanging then
      onTimeChanged(seconds)

  timeSlider.onMouseClicked = _ =>
    onTimeChanged(timeSlider.value.value)

  def getSelectedClip: Option[SelectedClip] = selectedClipProperty.value

  def selectClip(targetOpt: Option[SelectedClip]): Unit =
    selectedClipProperty.value = targetOpt
    targetOpt match
      case Some(SelectedClip.SelectedVideo(v)) =>
        toolbar.setSelectedEffect(v.effect)
        toolbar.setEffectControlsVisible(true)
      case _ =>
        toolbar.setSelectedEffect(VideoEffect.None)
        toolbar.setEffectControlsVisible(false)
    onClipSelected(targetOpt)
    currentTimelineProperty.value.foreach(render)

  def toggleVideoSelection(clip: VideoClip): Unit =
    val isAlreadySelected = selectedClipProperty.value.exists:
      case SelectedClip.SelectedVideo(v) => clip.isSameAs(v)
      case _                             => false

    if isAlreadySelected then selectClip(None)
    else selectClip(Some(SelectedClip.SelectedVideo(clip)))

  def toggleAudioSelection(clip: AudioClip): Unit =
    val isAlreadySelected = selectedClipProperty.value.exists:
      case SelectedClip.SelectedAudio(a) => clip.isSameAs(a)
      case _                             => false

    if isAlreadySelected then selectClip(None)
    else selectClip(Some(SelectedClip.SelectedAudio(clip)))

  def updateTimelineTime(seconds: Double): Unit =
    Platform.runLater:
      timeSlider.value = seconds

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

  def updateAudio(audioUrlOpt: Option[String], relativeTimeSeconds: Double, isPlaying: Boolean): Unit =
    audioPlayer.update(audioUrlOpt, relativeTimeSeconds, isPlaying)

  def render(timeline: Timeline): Unit =
    currentTimelineProperty.value = Some(timeline)
    val maxDuration = {
      val maxV = timeline.videoTracks.flatMap(_.clips).map(_.endTime).maxOption.getOrElse(0.0)
      val maxA = timeline.audioTracks.flatMap(_.clips).map(_.endTime).maxOption.getOrElse(0.0)
      Math.max(maxV, maxA)
    }
    timeSlider.max = Math.max(60.0, maxDuration)
    timelinePanel.draw(timeline, timeSlider.value.value, selectedClipProperty.value)