package app.controller

import scalafx.Includes.*
import core.model.*
import core.engine.*
import app.view.TimelineView
import app.view.components.SelectedClip
import scalafx.scene.layout.VBox
import java.util.concurrent.atomic.AtomicReference

private case class ControllerState(
                                    timeline: Timeline,
                                    currentTime: Double = 0.0,
                                    playerState: PlayerState = Paused
                                  )

class TimelineController:

  private val initialVideoTrack = VideoTrack(id = 1, clips = Nil)
  private val initialAudioTrack = AudioTrack(id = 1, clips = Nil)

  private val state = new AtomicReference[ControllerState](
    ControllerState(
      timeline = Timeline(
        videoTracks = List(initialVideoTrack),
        audioTracks = List(initialAudioTrack)
      )
    )
  )

  private val view = new TimelineView()

  private def findVideoTrack(id: Int): Option[VideoTrack] =
    state.get().timeline.videoTracks.find(_.id == id)

  private def findAudioTrack(id: Int): Option[AudioTrack] =
    state.get().timeline.audioTracks.find(_.id == id)

  private def getActiveVideoClip(): Option[VideoClip] =
    val current = state.get()
    current.timeline.videoTracks
      .flatMap(_.clips)
      .find(_.containsTime(current.currentTime))

  private def getActiveAudioClip(): Option[AudioClip] =
    val current = state.get()
    current.timeline.audioTracks
      .flatMap(_.clips)
      .find(_.containsTime(current.currentTime))

  private def syncMediaPlayback(): Unit =
    val current = state.get()
    val isPlaying = current.playerState match
      case Playing(_) => true
      case Paused     => false

    getActiveVideoClip() match
      case Some(clip) =>
        view.updatePreview(Some(clip.sourceUrl), clip.relativeTimeAt(current.currentTime), isPlaying)
      case None =>
        view.updatePreview(None, 0.0, false)

    getActiveAudioClip() match
      case Some(clip) =>
        view.updateAudio(Some(clip.sourceUrl), clip.relativeTimeAt(current.currentTime), isPlaying)
      case None =>
        view.updateAudio(None, 0.0, false)

  private def totalTimelineDuration: Double =
    val current = state.get()
    val maxVideo = current.timeline.videoTracks.flatMap(_.clips).map(_.endTime).maxOption.getOrElse(0.0)
    val maxAudio = current.timeline.audioTracks.flatMap(_.clips).map(_.endTime).maxOption.getOrElse(0.0)
    Math.max(maxVideo, maxAudio)

  private val inputHandler = new InputHandler(onTogglePlayback = () => {
    val current = state.get()
    val targetTime = if current.currentTime >= totalTimelineDuration && totalTimelineDuration > 0 then
      view.updateTimelineTime(0.0)
      0.0
    else current.currentTime

    val nextState = current.playerState match
      case Paused     => Playing(speed = 1.0)
      case Playing(_) => Paused

    state.set(current.copy(currentTime = targetTime, playerState = nextState))
    syncMediaPlayback()
  })

  private def calculateTimelineAfterDelete(): Timeline =
    val current = state.get()
    view.getSelectedClip match
      case Some(SelectedClip.SelectedVideo(selVideo)) =>
        findVideoTrack(1).fold(current.timeline) { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selVideo))
          if idx != -1 then TimelineEngine.removeVideoClip(current.timeline, 1, idx)
          else current.timeline
        }

      case Some(SelectedClip.SelectedAudio(selAudio)) =>
        findAudioTrack(1).fold(current.timeline) { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selAudio))
          if idx != -1 then TimelineEngine.removeAudioClip(current.timeline, 1, idx)
          else current.timeline
        }

      case None =>
        val videoIdx = findVideoTrack(1).map(_.clips.indexWhere(_.containsTime(current.currentTime))).getOrElse(-1)
        val audioIdx = findAudioTrack(1).map(_.clips.indexWhere(_.containsTime(current.currentTime))).getOrElse(-1)

        val timelineAfterVideo = if videoIdx != -1 then
          TimelineEngine.removeVideoClip(current.timeline, 1, videoIdx)
        else current.timeline

        if audioIdx != -1 then
          TimelineEngine.removeAudioClip(timelineAfterVideo, 1, audioIdx)
        else timelineAfterVideo

  private def calculateTimelineAfterCut(cursorTime: Double): Timeline =
    val current = state.get()
    view.getSelectedClip match
      case Some(SelectedClip.SelectedVideo(selVideo)) =>
        findVideoTrack(1).fold(current.timeline) { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selVideo))
          track.clips.lift(idx)
            .filter(_.containsTime(cursorTime))
            .fold(current.timeline)(c => TimelineEngine.cutVideoClip(current.timeline, 1, idx, cursorTime - c.startTime))
        }

      case Some(SelectedClip.SelectedAudio(selAudio)) =>
        findAudioTrack(1).fold(current.timeline) { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selAudio))
          track.clips.lift(idx)
            .filter(_.containsTime(cursorTime))
            .fold(current.timeline)(c => TimelineEngine.cutAudioClip(current.timeline, 1, idx, cursorTime - c.startTime))
        }

      case None =>
        val videoCutTimeline = findVideoTrack(1).fold(current.timeline) { track =>
          val videoIdx = track.clips.indexWhere(_.containsTime(cursorTime))
          if videoIdx != -1 then
            TimelineEngine.cutVideoClip(current.timeline, 1, videoIdx, cursorTime - track.clips(videoIdx).startTime)
          else current.timeline
        }

        findAudioTrack(1).fold(videoCutTimeline) { track =>
          val audioIdx = track.clips.indexWhere(_.containsTime(cursorTime))
          if audioIdx != -1 then
            TimelineEngine.cutAudioClip(videoCutTimeline, 1, audioIdx, cursorTime - track.clips(audioIdx).startTime)
          else videoCutTimeline
        }

  view.onClipSelected = _ => ()

  view.onImportRequested = { () =>
    val currentWindow = view.getScene.getWindow
    app.utils.MediaImporter.chooseVideoFile(currentWindow).foreach { case (file, durataReale) =>
      val current = state.get()
      val fileUrl = file.toURI.toString

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
        videoTrackId = 1,
        audioTrackId = 1,
        videoClip = importedClip
      )

      state.set(current.copy(timeline = updatedTimeline))
      view.render(updatedTimeline)
      syncMediaPlayback()
    }
  }

  view.onDeleteRequested = { () =>
    val updatedTimeline = calculateTimelineAfterDelete()
    state.set(state.get().copy(timeline = updatedTimeline))
    view.selectClip(None)
    view.render(updatedTimeline)
    syncMediaPlayback()
  }

  view.onCutRequested = { cursorTime =>
    val updatedTimeline = calculateTimelineAfterCut(cursorTime)
    state.set(state.get().copy(timeline = updatedTimeline))
    view.render(updatedTimeline)
    syncMediaPlayback()
  }

  view.onSnapRequested = { () =>
    val updatedTimeline = TimelineEngine.snapAllTracks(state.get().timeline, 1, 1)
    state.set(state.get().copy(timeline = updatedTimeline))
    view.render(updatedTimeline)
    syncMediaPlayback()
  }

  view.onClipMoved = { (clip, newTime) =>
    val current = state.get()
    val updatedTimeline = TimelineEngine.moveClip(current.timeline, clip, newTime)
    state.set(current.copy(timeline = updatedTimeline))
    view.render(updatedTimeline)
    syncMediaPlayback()
  }

  view.onTimeChanged = { newCursorTime =>
    state.set(state.get().copy(currentTime = newCursorTime))
    syncMediaPlayback()
  }

  view.onVideoTimeUpdated = { newVideoTime =>
    val current = state.get()
    val previousClip = getActiveVideoClip()

    val nextTime = previousClip.map { clip =>
      val calculatedTime = clip.startTime + (newVideoTime - clip.trimStart)
      if calculatedTime >= current.currentTime then
        view.updateTimelineTime(calculatedTime)
        calculatedTime
      else current.currentTime
    }.getOrElse(current.currentTime)

    state.set(current.copy(currentTime = nextTime))
    val currentClip = getActiveVideoClip()

    if previousClip != currentClip then
      currentClip match
        case Some(_) =>
          syncMediaPlayback()
        case None =>
          view.updatePreview(None, 0.0, false)
          getActiveAudioClip() match
            case Some(aClip) =>
              val isPlaying = state.get().playerState match
                case Playing(_) => true
                case Paused     => false
              view.updateAudio(Some(aClip.sourceUrl), aClip.relativeTimeAt(nextTime), isPlaying)
            case None =>
              view.updateAudio(None, 0.0, false)
  }

  view.onTogglePlaybackRequested = { () =>
    val current = state.get()
    val targetTime = if current.currentTime >= totalTimelineDuration && totalTimelineDuration > 0 then
      view.updateTimelineTime(0.0)
      0.0
    else current.currentTime

    val nextState = current.playerState match
      case Paused     => Playing(speed = 1.0)
      case Playing(_) => Paused

    state.set(current.copy(currentTime = targetTime, playerState = nextState))
    syncMediaPlayback()
  }

  view.onKeyReleased = (event: scalafx.scene.input.KeyEvent) => inputHandler.handleKeyEvent(event)

  def viewComponent: VBox = view

  view.render(state.get().timeline)
  syncMediaPlayback()