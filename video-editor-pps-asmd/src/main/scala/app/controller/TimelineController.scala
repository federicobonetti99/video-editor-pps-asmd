package app.controller

import scalafx.Includes.*
import core.model.*
import core.engine.*
import app.view.TimelineView
import app.view.components.SelectedClip
import scalafx.scene.layout.VBox

class TimelineController:

  private val initialVideoTrack = VideoTrack(id = 1, clips = Nil)
  private val initialAudioTrack = AudioTrack(id = 1, clips = Nil)

  private var currentTimeline = Timeline(
    videoTracks = List(initialVideoTrack),
    audioTracks = List(initialAudioTrack)
  )

  private var currentTime: Double = 0.0
  private var currentPlayerState: PlayerState = Paused

  private val view = new TimelineView()

  private def findVideoTrack(id: Int): Option[VideoTrack] =
    currentTimeline.videoTracks.find(_.id == id)

  private def findAudioTrack(id: Int): Option[AudioTrack] =
    currentTimeline.audioTracks.find(_.id == id)

  private def getActiveVideoClip(): Option[VideoClip] =
    currentTimeline.videoTracks
      .flatMap(_.clips)
      .find(_.containsTime(currentTime))

  private def getActiveAudioClip(): Option[AudioClip] =
    currentTimeline.audioTracks
      .flatMap(_.clips)
      .find(_.containsTime(currentTime))

  private def syncMediaPlayback(): Unit =
    val isPlaying = currentPlayerState match
      case Playing(_) => true
      case Paused     => false

    getActiveVideoClip() match
      case Some(clip) =>
        view.updatePreview(Some(clip.sourceUrl), clip.relativeTimeAt(currentTime), isPlaying)
      case None =>
        view.updatePreview(None, 0.0, false)

    getActiveAudioClip() match
      case Some(clip) =>
        view.updateAudio(Some(clip.sourceUrl), clip.relativeTimeAt(currentTime), isPlaying)
      case None =>
        view.updateAudio(None, 0.0, false)

  private def totalTimelineDuration: Double =
    val maxVideo = currentTimeline.videoTracks.flatMap(_.clips).map(_.endTime).maxOption.getOrElse(0.0)
    val maxAudio = currentTimeline.audioTracks.flatMap(_.clips).map(_.endTime).maxOption.getOrElse(0.0)
    Math.max(maxVideo, maxAudio)

  private val inputHandler = new InputHandler(onTogglePlayback = () => {
    if currentTime >= totalTimelineDuration && totalTimelineDuration > 0 then
      currentTime = 0.0
      view.updateTimelineTime(0.0)

    currentPlayerState = currentPlayerState match
      case Paused     => Playing(speed = 1.0)
      case Playing(_) => Paused
    syncMediaPlayback()
  })

  private def calculateTimelineAfterDelete(): Timeline =
    view.getSelectedClip match
      case Some(SelectedClip.SelectedVideo(selVideo)) =>
        findVideoTrack(1).fold(currentTimeline) { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selVideo))
          if idx != -1 then TimelineEngine.removeVideoClip(currentTimeline, 1, idx)
          else currentTimeline
        }

      case Some(SelectedClip.SelectedAudio(selAudio)) =>
        findAudioTrack(1).fold(currentTimeline) { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selAudio))
          if idx != -1 then TimelineEngine.removeAudioClip(currentTimeline, 1, idx)
          else currentTimeline
        }

      case None =>
        val videoIdx = findVideoTrack(1).map(_.clips.indexWhere(_.containsTime(currentTime))).getOrElse(-1)
        val audioIdx = findAudioTrack(1).map(_.clips.indexWhere(_.containsTime(currentTime))).getOrElse(-1)

        val timelineAfterVideo = if videoIdx != -1 then
          TimelineEngine.removeVideoClip(currentTimeline, 1, videoIdx)
        else currentTimeline

        if audioIdx != -1 then
          TimelineEngine.removeAudioClip(timelineAfterVideo, 1, audioIdx)
        else timelineAfterVideo

  private def calculateTimelineAfterCut(cursorTime: Double): Timeline =
    view.getSelectedClip match
      case Some(SelectedClip.SelectedVideo(selVideo)) =>
        findVideoTrack(1).fold(currentTimeline) { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selVideo))
          track.clips.lift(idx)
            .filter(_.containsTime(cursorTime))
            .fold(currentTimeline)(c => TimelineEngine.cutVideoClip(currentTimeline, 1, idx, cursorTime - c.startTime))
        }

      case Some(SelectedClip.SelectedAudio(selAudio)) =>
        findAudioTrack(1).fold(currentTimeline) { track =>
          val idx = track.clips.indexWhere(_.isSameAs(selAudio))
          track.clips.lift(idx)
            .filter(_.containsTime(cursorTime))
            .fold(currentTimeline)(c => TimelineEngine.cutAudioClip(currentTimeline, 1, idx, cursorTime - c.startTime))
        }

      case None =>
        val videoCutTimeline = findVideoTrack(1).fold(currentTimeline) { track =>
          val videoIdx = track.clips.indexWhere(_.containsTime(cursorTime))
          if videoIdx != -1 then
            TimelineEngine.cutVideoClip(currentTimeline, 1, videoIdx, cursorTime - track.clips(videoIdx).startTime)
          else currentTimeline
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
      val fileUrl = file.toURI.toString

      val importedClip = VideoClip(
        sourceUrl = fileUrl,
        sourceLength = durataReale,
        startTime = currentTime,
        trimStart = 0.0,
        duration = durataReale,
        effect = VideoEffect.None
      )

      currentTimeline = TimelineEngine.importVideoWithAudio(
        timeline = currentTimeline,
        videoTrackId = 1,
        audioTrackId = 1,
        videoClip = importedClip
      )

      view.render(currentTimeline)
      syncMediaPlayback()
    }
  }

  view.onDeleteRequested = { () =>
    currentTimeline = calculateTimelineAfterDelete()
    view.selectClip(None)
    view.render(currentTimeline)
    syncMediaPlayback()
  }

  view.onCutRequested = { cursorTime =>
    currentTimeline = calculateTimelineAfterCut(cursorTime)
    view.render(currentTimeline)
    syncMediaPlayback()
  }

  view.onSnapRequested = { () =>
    currentTimeline = TimelineEngine.snapClipsTogether(currentTimeline, 1)
    view.render(currentTimeline)
    syncMediaPlayback()
  }

  view.onTimeChanged = { newCursorTime =>
    currentTime = newCursorTime
    syncMediaPlayback()
  }

  view.onVideoTimeUpdated = { newVideoTime =>
    val previousClip = getActiveVideoClip()

    previousClip.foreach { clip =>
      val calculatedTime = clip.startTime + (newVideoTime - clip.trimStart)
      if calculatedTime >= currentTime then
        currentTime = calculatedTime
        view.updateTimelineTime(currentTime)
    }

    val currentClip = getActiveVideoClip()

    if previousClip != currentClip then
      currentClip match
        case Some(_) =>
          syncMediaPlayback()
        case None =>
          view.updatePreview(None, 0.0, false)
          getActiveAudioClip() match
            case Some(aClip) =>
              val isPlaying = currentPlayerState match
                case Playing(_) => true
                case Paused     => false
              view.updateAudio(Some(aClip.sourceUrl), aClip.relativeTimeAt(currentTime), isPlaying)
            case None =>
              view.updateAudio(None, 0.0, false)
  }

  view.onTogglePlaybackRequested = { () =>
    if currentTime >= totalTimelineDuration && totalTimelineDuration > 0 then
      currentTime = 0.0
      view.updateTimelineTime(0.0)

    currentPlayerState = currentPlayerState match
      case Paused     => Playing(speed = 1.0)
      case Playing(_) => Paused
    syncMediaPlayback()
  }

  view.onKeyReleased = (event: scalafx.scene.input.KeyEvent) => inputHandler.handleKeyEvent(event)

  def viewComponent: VBox = view

  view.render(currentTimeline)
  syncMediaPlayback()