package app.controller

import app.utils.MediaImporter
import scalafx.Includes.*
import core.model.*
import core.engine.*
import app.view.TimelineView
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

  // Stato per memorizzare la clip selezionata: (mediaType, trackId, clipIndex)
  private var selectedClip: Option[(String, Int, Int)] = None

  private val view = new TimelineView()
  private val inputHandler = new InputHandler(onTogglePlayback = view.onTogglePlaybackRequested)

  private def getActiveClip(): Option[VideoClip] =
    currentTimeline.videoTracks
      .flatMap(_.clips)
      .find(c => currentTime >= c.startTime && currentTime < (c.startTime + c.duration))

  private def syncVideoPreview(): Unit =
    getActiveClip() match
      case Some(clip) =>
        val relativeTime = (currentTime - clip.startTime) + clip.trimStart
        val isPlaying = currentPlayerState match
          case Playing(_) => true
          case Paused => false
        view.updatePreview(Some(clip.sourceUrl), relativeTime, isPlaying)
      case None =>
        view.updatePreview(None, 0.0, false)


  view.onClipSelected = { (mediaType, trackId, clipIndex) =>
    val clickedClip = (mediaType, trackId, clipIndex)

    if selectedClip.contains(clickedClip) then
      println(s"🔄 Clip deselezionata: $mediaType nella traccia $trackId all'indice $clipIndex")
      selectedClip = None
    else
      println(s"📌 Nuova clip selezionata: $mediaType nella traccia $trackId all'indice $clipIndex")
      selectedClip = Some(clickedClip)

    view.render(currentTimeline, selectedClip)
  }

  view.onImportRequested = { () =>
    val window: scalafx.stage.Window = view.scene.value.window.value

    MediaImporter.chooseVideoFile(window).foreach { (file, duration) =>
      val importedVideo = VideoClip(
        sourceUrl = file.toURI.toString,
        sourceLength = duration,
        startTime = 0.0,
        trimStart = 0.0,
        duration = duration,
        effect = VideoEffect.None
      )

      currentTimeline = TimelineEngine.importVideoWithAudio(
        timeline = currentTimeline,
        videoTrackId = 1,
        audioTrackId = 1,
        videoClip = importedVideo
      )

      view.render(currentTimeline, selectedClip)
      syncVideoPreview()
    }
  }

  view.onDeleteRequested = { () =>
    val videoTrack = currentTimeline.videoTracks.find(_.id == 1).get
    val clipIndexOpt = videoTrack.clips.indexWhere { c =>
      currentTime >= c.startTime && currentTime < (c.startTime + c.duration)
    }

    if clipIndexOpt != -1 then
      var newTimeline = TimelineEngine.removeVideoClip(currentTimeline, 1, clipIndexOpt)
      newTimeline = TimelineEngine.removeAudioClip(newTimeline, 1, clipIndexOpt)

      currentTimeline = newTimeline
      selectedClip = None
      view.render(currentTimeline, selectedClip)
      syncVideoPreview()
  }

  view.onCutRequested = { cursorTime =>
    val videoTrack = currentTimeline.videoTracks.find(_.id == 1).get
    val clipIndexOpt = videoTrack.clips.indexWhere { c =>
      cursorTime >= c.startTime && cursorTime < (c.startTime + c.duration)
    }

    if clipIndexOpt != -1 then
      val targetClip = videoTrack.clips(clipIndexOpt)
      val relativeCut = cursorTime - targetClip.startTime

      var newTimeline = TimelineEngine.cutVideoClip(currentTimeline, 1, clipIndexOpt, relativeCut)
      newTimeline = TimelineEngine.cutAudioClip(newTimeline, 1, clipIndexOpt, relativeCut)

      currentTimeline = newTimeline
      selectedClip = None
      view.render(currentTimeline, selectedClip)
      syncVideoPreview()
  }

  view.onSnapRequested = { () =>
    currentTimeline = TimelineEngine.snapClipsTogether(currentTimeline, 1)
    view.render(currentTimeline, selectedClip)
    syncVideoPreview()
  }

  view.onTimeChanged = { newCursorTime =>
    currentTime = newCursorTime
    syncVideoPreview()
  }

  view.onVideoTimeUpdated = { newVideoTime =>
    val previousClip = getActiveClip()

    previousClip match
      case Some(clip) =>
        currentTime = clip.startTime + newVideoTime - clip.trimStart
        view.updateTimelineTime(currentTime)
      case None => ()

    val currentClip = getActiveClip()

    if previousClip != currentClip then
      currentClip match
        case Some(newClip) =>
          val relativeTime = (currentTime - newClip.startTime) + newClip.trimStart
          val isPlaying = currentPlayerState match
            case Playing(_) => true
            case Paused => false

          view.updatePreview(Some(newClip.sourceUrl), relativeTime, isPlaying)

        case None =>
          view.updatePreview(None, 0.0, false)
  }

  view.onTogglePlaybackRequested = { () =>
    currentPlayerState = currentPlayerState match
      case Paused => Playing(speed = 1.0)
      case Playing(_) => Paused
    syncVideoPreview()
  }

  view.onKeyReleased = (event: scalafx.scene.input.KeyEvent) => inputHandler.handleKeyEvent(event)

  def viewComponent: VBox = view

  view.render(currentTimeline, selectedClip)
  syncVideoPreview()