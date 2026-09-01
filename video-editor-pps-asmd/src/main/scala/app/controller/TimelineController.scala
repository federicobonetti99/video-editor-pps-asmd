package app.controller

import app.utils.MediaImporter
import scalafx.Includes.*
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Alert, Label, ProgressBar}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.layout.VBox
import scalafx.stage.{FileChooser, Modality, Stage, StageStyle}
import core.model.*
import core.engine.*
import app.view.TimelineView
import app.view.components.{ActiveAudioTrackInfo, SelectedClip}
import scalafx.animation.AnimationTimer
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

private case class ControllerState(
                                    history: History[Timeline],
                                    currentTime: Double = 0.0,
                                    playerState: PlayerState = Paused,
                                    lastFrameTimeNanos: Long = 0L
                                  ):
  def timeline: Timeline = history.current

class TimelineController:

  private val initialVideoTrack = VideoTrack(id = 1, clips = Nil)
  private val initialAudioTrack = AudioTrack(id = 1, clips = Nil)

  private val initialTimeline = Timeline(
    videoTracks = List(initialVideoTrack),
    audioTracks = List(initialAudioTrack)
  )

  private val state = new AtomicReference[ControllerState](
    ControllerState(
      history = History(current = initialTimeline)
    )
  )

  private val view: TimelineView = new TimelineView(
    onDeleteRequested = () => handleDelete(),
    onCutRequested = cursorTime => handleCut(cursorTime),
    onSnapRequested = () => handleSnap(),
    onTogglePlaybackRequested = () => handleTogglePlayback(),
    onTimeChanged = newCursorTime => handleTimeChanged(newCursorTime),
    onImportRequested = () => handleImport(),
    onExportRequested = () => handleExport(),
    onUndoRequested = () => handleUndo(),
    onRedoRequested = () => handleRedo(),
    onVideoTimeUpdated = _ => (),
    onClipSelected = _ => (),
    onClipMoved = (clip, targetTrackId, newTime) => handleClipMoved(clip, targetTrackId, newTime),
    onEffectSelected = effect => handleEffectApplied(effect),
    onAddVideoTrackRequested = () => handleAddVideoTrack(),
    onAddAudioTrackRequested = () => handleAddAudioTrack()
  )

  private val inputHandler = new InputHandler(
    onTogglePlayback = () => handleTogglePlayback(),
    onUndo = () => handleUndo(),
    onRedo = () => handleRedo()
  )

  view.onKeyReleased = (event: scalafx.scene.input.KeyEvent) => inputHandler.handleKeyEvent(event)

  private val masterTimer: AnimationTimer = AnimationTimer: now =>
    val current = state.get()
    if current.lastFrameTimeNanos > 0L then
      val deltaSeconds = (now - current.lastFrameTimeNanos) / 1e9
      current.playerState match
        case Playing(speed) =>
          val updatedTime = current.currentTime + (deltaSeconds * speed)
          val totalDuration = totalTimelineDuration

          if totalDuration > 0.0 && updatedTime >= totalDuration then
            state.set(current.copy(currentTime = totalDuration, playerState = Paused, lastFrameTimeNanos = 0L))
            view.updateTimelineTime(totalDuration)
            masterTimer.stop()
            syncMediaPlayback()
          else
            val prevVideo = getActiveVideoClip()
            val prevAudios = getActiveAudioClips()
            state.set(current.copy(currentTime = updatedTime, lastFrameTimeNanos = now))
            view.updateTimelineTime(updatedTime)

            val currVideo = getActiveVideoClip()
            val currAudios = getActiveAudioClips()

            val hasDynamicEffect = currVideo.exists { c =>
              c.effect match
                case VideoEffect.ZoomIn(_) | VideoEffect.Shake(_, _) | VideoEffect.FadeIn(_) => true
                case _ => false
            }

            if prevVideo != currVideo || prevAudios != currAudios || hasDynamicEffect then
              syncMediaPlayback()
        case Paused =>
          masterTimer.stop()
          state.set(current.copy(lastFrameTimeNanos = 0L))
    else
      state.set(current.copy(lastFrameTimeNanos = now))

  private def updateTimelineState(newTimeline: Timeline): Unit =
    val current = state.get()
    val newHistory = current.history.push(newTimeline)
    state.set(current.copy(history = newHistory))
    view.render(newTimeline)
    view.updateHistoryControls(newHistory.canUndo, newHistory.canRedo)
    syncMediaPlayback()

  private def handleUndo(): Unit =
    val current = state.get()
    if current.history.canUndo then
      val newHistory = current.history.undo()
      state.set(current.copy(history = newHistory))
      view.selectClip(None)
      view.render(newHistory.current)
      view.updateHistoryControls(newHistory.canUndo, newHistory.canRedo)
      syncMediaPlayback()

  private def handleRedo(): Unit =
    val current = state.get()
    if current.history.canRedo then
      val newHistory = current.history.redo()
      state.set(current.copy(history = newHistory))
      view.selectClip(None)
      view.render(newHistory.current)
      view.updateHistoryControls(newHistory.canUndo, newHistory.canRedo)
      syncMediaPlayback()

  private def findVideoTrack(id: Int): Option[VideoTrack] =
    state.get().timeline.videoTracks.find(_.id == id)

  private def findAudioTrack(id: Int): Option[AudioTrack] =
    state.get().timeline.audioTracks.find(_.id == id)

  private def getActiveVideoClip(): Option[VideoClip] =
    val current = state.get()
    current.timeline.videoTracks
      .sortBy(_.id)
      .reverse
      .flatMap(_.clips)
      .find(_.containsTime(current.currentTime))

  private def getActiveAudioClips(): List[AudioClip] =
    val current = state.get()
    current.timeline.audioTracks
      .flatMap(_.clips.find(_.containsTime(current.currentTime)))

  private def syncMediaPlayback(): Unit =
    val current = state.get()
    val isPlaying = current.playerState match
      case Playing(_) => true
      case Paused     => false

    getActiveVideoClip() match
      case Some(clip) =>
        view.updatePreview(
          Some(clip.sourceUrl),
          clip.relativeTimeAt(current.currentTime),
          isPlaying,
          clip.effect,
          clip.duration
        )
      case None =>
        view.updatePreview(None, 0.0, false, VideoEffect.None, 0.0)

    val activeAudiosInfo = getActiveAudioClips().map: clip =>
      ActiveAudioTrackInfo(
        sourceUrl = clip.sourceUrl,
        relativeTimeSeconds = clip.relativeTimeAt(current.currentTime)
      )

    view.updateAudio(activeAudiosInfo, isPlaying)

  private def totalTimelineDuration: Double =
    val current = state.get()
    val maxVideo = current.timeline.videoTracks.flatMap(_.clips).map(_.endTime).maxOption.getOrElse(0.0)
    val maxAudio = current.timeline.audioTracks.flatMap(_.clips).map(_.endTime).maxOption.getOrElse(0.0)
    Math.max(maxVideo, maxAudio)

  private def handleTogglePlayback(): Unit =
    val current = state.get()
    val totalDuration = totalTimelineDuration
    val targetTime = if totalDuration > 0.0 && current.currentTime >= totalDuration then
      view.updateTimelineTime(0.0)
      0.0
    else current.currentTime

    val nextState = current.playerState match
      case Paused     => Playing(speed = 1.0)
      case Playing(_) => Paused

    state.set(current.copy(currentTime = targetTime, playerState = nextState, lastFrameTimeNanos = 0L))

    nextState match
      case Playing(_) =>
        masterTimer.start()
      case Paused =>
        masterTimer.stop()

    syncMediaPlayback()

  private def handleImport(): Unit =
    val currentWindow = view.getScene.getWindow
    MediaImporter.chooseVideoFile(currentWindow).foreach { case (file, durataReale) =>
      val current = state.get()
      val fileUrl = file.toURI.toString

      val targetVideoTrackId = current.timeline.videoTracks.headOption.map(_.id).getOrElse(1)
      val targetAudioTrackId = current.timeline.audioTracks.headOption.map(_.id).getOrElse(1)

      val importedClip = VideoClip(
        sourceUrl = fileUrl,
        sourceLength = durataReale,
        startTime = current.currentTime,
        trimStart = 0.0,
        duration = durataReale,
        effect = VideoEffect.None
      )

      val updatedTimeline = TimelineEngine.importVideoWithAudio(
        timeline = current.timeline,
        videoTrackId = targetVideoTrackId,
        audioTrackId = targetAudioTrackId,
        videoClip = importedClip
      )

      updateTimelineState(updatedTimeline)
    }

  private def handleExport(): Unit =
    val current = state.get()
    val currentWindow = view.getScene.getWindow
    val fileChooser = new FileChooser:
      title = "Export Timeline Video"
      extensionFilters.add(new FileChooser.ExtensionFilter("MP4 Video (*.mp4)", "*.mp4"))

    val targetFile = fileChooser.showSaveDialog(currentWindow)
    if targetFile != null then
      val settings = ExportSettings(outputFile = targetFile)
      val exporter = VideoExporter()

      val progressLabel = new Label("Preparazione esportazione..."):
        style = "-fx-font-size: 14px; -fx-font-weight: bold;"

      val progressBar = new ProgressBar:
        progress = 0.0
        prefWidth = 320
        prefHeight = 22

      val dialogStage = new Stage:
        title = "Esportazione Video"
        initModality(Modality.ApplicationModal)
        if currentWindow != null then initOwner(currentWindow)
        resizable = false
        scene = new Scene(360, 120):
          root = new VBox(15, progressLabel, progressBar):
            padding = Insets(20)
            alignment = Pos.Center

      dialogStage.show()

      Future {
        exporter.exportVideo(
          timeline = current.timeline,
          settings = settings,
          onProgress = { fraction =>
            Platform.runLater {
              progressBar.progress = fraction
              val percent = Math.min(100, (fraction * 100).toInt)
              progressLabel.text = s"Esportazione in corso: $percent%"
            }
          }
        )
      }.flatMap(res => Future.fromTry(res)).onComplete {
        case Success(file) =>
          Platform.runLater {
            dialogStage.close()
            val alert = new Alert(AlertType.Information) {
              title = "Export Success"
              headerText = "Video Exported Successfully"
              contentText = s"Saved to: ${file.getAbsolutePath}"
            }
            alert.showAndWait()
          }
        case Failure(exception) =>
          Platform.runLater {
            dialogStage.close()
            val alert = new Alert(AlertType.Error) {
              title = "Export Failed"
              headerText = "FFmpeg Export Failed"
              contentText = exception.getMessage
            }
            alert.showAndWait()
          }
      }

  private def handleAddVideoTrack(): Unit =
    val updatedTimeline = TimelineEngine.addVideoTrack(state.get().timeline)
    updateTimelineState(updatedTimeline)

  private def handleAddAudioTrack(): Unit =
    val updatedTimeline = TimelineEngine.addAudioTrack(state.get().timeline)
    updateTimelineState(updatedTimeline)

  private def handleDelete(): Unit =
    val updatedTimeline = calculateTimelineAfterDelete()
    view.selectClip(None)
    updateTimelineState(updatedTimeline)

  private def handleCut(cursorTime: Double): Unit =
    val updatedTimeline = calculateTimelineAfterCut(cursorTime)
    updateTimelineState(updatedTimeline)

  private def handleSnap(): Unit =
    val updatedTimeline = TimelineEngine.snapAllTracks(state.get().timeline)
    updateTimelineState(updatedTimeline)

  private def handleClipMoved(clip: MediaClip, targetTrackId: Int, newTime: Double): Unit =
    val current = state.get()
    val updatedTimeline = TimelineEngine.moveClipToTrack(current.timeline, clip, targetTrackId, newTime)
    updateTimelineState(updatedTimeline)

  private def handleTimeChanged(newCursorTime: Double): Unit =
    state.set(state.get().copy(currentTime = newCursorTime))
    syncMediaPlayback()

  private def handleEffectApplied(effect: VideoEffect): Unit =
    val current = state.get()
    view.getSelectedClip match
      case Some(SelectedClip.SelectedVideo(trackId, selVideo)) =>
        findVideoTrack(trackId).foreach { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selVideo))
          if idx != -1 then
            val updatedTimeline = TimelineEngine.applyEffectToVideoClip(current.timeline, trackId, idx, effect)
            val updatedClip = track.clips(idx).copy(effect = effect)
            view.selectClip(Some(SelectedClip.SelectedVideo(trackId, updatedClip)))
            updateTimelineState(updatedTimeline)
        }
      case _ => ()

  private def calculateTimelineAfterDelete(): Timeline =
    val current = state.get()
    view.getSelectedClip match
      case Some(SelectedClip.SelectedVideo(trackId, selVideo)) =>
        findVideoTrack(trackId).fold(current.timeline) { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selVideo))
          if idx != -1 then TimelineEngine.removeVideoClip(current.timeline, trackId, idx)
          else current.timeline
        }

      case Some(SelectedClip.SelectedAudio(trackId, selAudio)) =>
        findAudioTrack(trackId).fold(current.timeline) { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selAudio))
          if idx != -1 then TimelineEngine.removeAudioClip(current.timeline, trackId, idx)
          else current.timeline
        }

      case None =>
        val timelineAfterVideo = current.timeline.videoTracks.foldLeft(current.timeline) { (acc, track) =>
          val videoIdx = track.clips.indexWhere(_.containsTime(current.currentTime))
          if videoIdx != -1 then TimelineEngine.removeVideoClip(acc, track.id, videoIdx)
          else acc
        }

        current.timeline.audioTracks.foldLeft(timelineAfterVideo) { (acc, track) =>
          val audioIdx = track.clips.indexWhere(_.containsTime(current.currentTime))
          if audioIdx != -1 then TimelineEngine.removeAudioClip(acc, track.id, audioIdx)
          else acc
        }

  private def calculateTimelineAfterCut(cursorTime: Double): Timeline =
    val current = state.get()
    view.getSelectedClip match
      case Some(SelectedClip.SelectedVideo(trackId, selVideo)) =>
        findVideoTrack(trackId).fold(current.timeline) { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selVideo))
          track.clips.lift(idx)
            .filter(_.containsTime(cursorTime))
            .fold(current.timeline)(c => TimelineEngine.cutVideoClip(current.timeline, trackId, idx, cursorTime - c.startTime))
        }

      case Some(SelectedClip.SelectedAudio(trackId, selAudio)) =>
        findAudioTrack(trackId).fold(current.timeline) { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selAudio))
          track.clips.lift(idx)
            .filter(_.containsTime(cursorTime))
            .fold(current.timeline)(c => TimelineEngine.cutAudioClip(current.timeline, trackId, idx, cursorTime - c.startTime))
        }

      case None =>
        val videoCutTimeline = current.timeline.videoTracks.foldLeft(current.timeline) { (acc, track) =>
          val videoIdx = track.clips.indexWhere(_.containsTime(cursorTime))
          if videoIdx != -1 then
            TimelineEngine.cutVideoClip(acc, track.id, videoIdx, cursorTime - track.clips(videoIdx).startTime)
          else acc
        }

        current.timeline.audioTracks.foldLeft(videoCutTimeline) { (acc, track) =>
          val audioIdx = track.clips.indexWhere(_.containsTime(cursorTime))
          if audioIdx != -1 then
            TimelineEngine.cutAudioClip(acc, track.id, audioIdx, cursorTime - track.clips(audioIdx).startTime)
          else acc
        }

  def viewComponent: VBox = view

  view.render(state.get().timeline)
  view.updateHistoryControls(canUndo = false, canRedo = false)
  syncMediaPlayback()