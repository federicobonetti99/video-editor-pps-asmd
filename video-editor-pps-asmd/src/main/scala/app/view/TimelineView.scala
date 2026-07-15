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

  // 1. DICHIARIAMO PRIMA TUTTI I COMPONENTI INDIPENDENTI
  private val preview = new VideoPreview(480.0, 270.0)
  private val timelinePanel = new TimelinePanel()

  private val timeSlider = new Slider {
    min = 0.0
    max = 60.0
    value = 0.0
    prefWidth = 600
    maxWidth = 800
  }

  // 2. ORA DICHIARIAMO LA TOOLBAR (che fa riferimento a timeSlider in modo sicuro)
  private val toolbar = new ToolbarControls(
    onImport = () => onImportRequested(),
    onDelete = () => onDeleteRequested(),
    onCut = () => onCutRequested(timeSlider.value.value),
    onSnap = () => onSnapRequested(),
    onPlay = () => onTogglePlaybackRequested()
  )

  // 3. ASSEGNAZIONE DEI CHILDREN ALLA FINE
  children = Seq(preview, timeSlider, timelinePanel, toolbar)

  timeSlider.valueProperty.addListener { (_, _, newValue) =>
    val seconds = newValue.doubleValue()
    toolbar.updateTimeLabel(seconds)
    timelinePanel.updatePlayhead(seconds)

    if timeSlider.isFocused then
      onTimeChanged(seconds)
  }

  def updateTimelineTime(seconds: Double): Unit =
    Platform.runLater { timeSlider.value = seconds }

  def updatePreview(videoUrlOpt: Option[String], relativeTimeSeconds: Double, isPlaying: Boolean): Unit =
    preview.update(videoUrlOpt, relativeTimeSeconds, isPlaying, onVideoTimeUpdated)

  def render(timeline: Timeline): Unit =
    timelinePanel.draw(timeline, timeSlider.value.value)