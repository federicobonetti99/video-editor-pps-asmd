package app.view

import scalafx.Includes.*
import scalafx.application.Platform
import scalafx.scene.control.Slider
import scalafx.scene.layout.VBox
import scalafx.scene.input.{KeyEvent, KeyCode}
import core.model.*
import app.view.components.*

class TimelineView extends VBox:

  spacing = 15
  style = "-fx-padding: 15; -fx-background-color: #1a1a1a;"

  var onDeleteRequested: () => Unit = () => ()
  var onCutRequested: Double => Unit = _ => ()
  var onSnapRequested: () => Unit = () => ()
  var onTogglePlaybackRequested: () => Unit = () => ()
  var onTimeChanged: Double => Unit = _ => ()
  var onImportRequested: () => Unit = () => ()
  var onVideoTimeUpdated: Double => Unit = _ => ()
  var onClipSelected: Option[SelectedClip] => Unit = _ => ()

  private var selectedClipOpt: Option[SelectedClip] = None
  private var currentTimelineRef: Option[Timeline] = None

  private val preview = new VideoPreview(480.0, 270.0)
  private val timelinePanel = new TimelinePanel()

  timelinePanel.onVideoClipClicked = { clip =>
    toggleVideoSelection(clip)
  }

  timelinePanel.onAudioClipClicked = { clip =>
    toggleAudioSelection(clip)
  }

  private val timeSlider = new Slider {
    min = 0.0
    max = 60.0
    value = 0.0
    prefWidth = 600
    maxWidth = 800
  }

  private val toolbar = new ToolbarControls(
    onImport = () => onImportRequested(),
    onDelete = () => onDeleteRequested(),
    onCut = () => onCutRequested(timeSlider.value.value),
    onSnap = () => onSnapRequested(),
    onPlay = () => onTogglePlaybackRequested()
  )

  children = Seq(preview, timeSlider, timelinePanel, toolbar)

  timeSlider.valueProperty.addListener { (_, _, newValue) =>
    val seconds = newValue.doubleValue()
    toolbar.updateTimeLabel(seconds)
    timelinePanel.updatePlayhead(seconds)

    if timeSlider.isFocused then
      onTimeChanged(seconds)
  }

  def getSelectedClip: Option[SelectedClip] = selectedClipOpt

  def selectClip(targetOpt: Option[SelectedClip]): Unit =
    selectedClipOpt = targetOpt
    onClipSelected(selectedClipOpt)
    currentTimelineRef.foreach(render)

  def toggleVideoSelection(clip: VideoClip): Unit =
    val isAlreadySelected = selectedClipOpt.exists {
      case SelectedClip.SelectedVideo(v) => v.sourceUrl == clip.sourceUrl && Math.abs(v.startTime - clip.startTime) < 0.001
      case _ => false
    }
    if isAlreadySelected then
      selectClip(None)
    else
      selectClip(Some(SelectedClip.SelectedVideo(clip)))

  def toggleAudioSelection(clip: AudioClip): Unit =
    val isAlreadySelected = selectedClipOpt.exists {
      case SelectedClip.SelectedAudio(a) => a.sourceUrl == clip.sourceUrl && Math.abs(a.startTime - clip.startTime) < 0.001
      case _ => false
    }
    if isAlreadySelected then
      selectClip(None)
    else
      selectClip(Some(SelectedClip.SelectedAudio(clip)))

  def updateTimelineTime(seconds: Double): Unit =
    Platform.runLater { timeSlider.value = seconds }

  def updatePreview(videoUrlOpt: Option[String], relativeTimeSeconds: Double, isPlaying: Boolean): Unit =
    preview.update(videoUrlOpt, relativeTimeSeconds, isPlaying, onVideoTimeUpdated)

  def render(timeline: Timeline): Unit =
    currentTimelineRef = Some(timeline)
    timelinePanel.draw(timeline, timeSlider.value.value, selectedClipOpt)