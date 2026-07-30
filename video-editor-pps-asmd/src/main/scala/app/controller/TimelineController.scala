package app.controller

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

  private val view = new TimelineView()

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
          case Paused     => false
        view.updatePreview(Some(clip.sourceUrl), relativeTime, isPlaying)
      case None =>
        view.updatePreview(None, 0.0, false)

  private val inputHandler = new InputHandler(onTogglePlayback = () => {
    currentPlayerState = currentPlayerState match
      case Paused     => Playing(speed = 1.0)
      case Playing(_) => Paused
    syncVideoPreview()
  })

  view.onImportRequested = { () =>
    val currentWindow = view.getScene.getWindow
    app.utils.MediaImporter.chooseVideoFile(currentWindow) match
      case Some((file, durataReale)) =>
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
        syncVideoPreview()
      case None =>
        println("🟡 Selezione annullata.")
  }

  view.onDeleteRequested = { () =>
    val videoTrack = currentTimeline.videoTracks.find(_.id == 1).get
    val clipIndexOpt = videoTrack.clips.indexWhere { c =>
      currentTime >= c.startTime && currentTime < (c.startTime + c.duration)
    }

    if clipIndexOpt != -1 then
      println(s"🗑️ Eliminazione della clip video e audio all'indice: $clipIndexOpt")
      var newTimeline = TimelineEngine.removeVideoClip(currentTimeline, trackId = 1, clipIndex = clipIndexOpt)
      newTimeline = TimelineEngine.removeAudioClip(newTimeline, trackId = 1, clipIndex = clipIndexOpt)

      currentTimeline = newTimeline
      view.render(currentTimeline)
      syncVideoPreview()
    else
      println("⚠️ Nessuna clip sotto il cursore da eliminare.")
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
      view.render(currentTimeline)
      syncVideoPreview()
  }

  view.onSnapRequested = { () =>
    var newTimeline = TimelineEngine.snapClipsTogether(currentTimeline, 1)
    currentTimeline = newTimeline
    view.render(currentTimeline)
    syncVideoPreview()
  }

  view.onTimeChanged = { newCursorTime =>
    currentTime = newCursorTime
    syncVideoPreview()
  }

  view.onVideoTimeUpdated = { newVideoTime =>
    val previousClip = getActiveClip()

    previousClip.foreach { clip =>
      val calculatedTime = clip.startTime + (newVideoTime - clip.trimStart)

      if calculatedTime >= currentTime then
        currentTime = calculatedTime
        view.updateTimelineTime(currentTime)
    }

    val currentClip = getActiveClip()

    if previousClip != currentClip then
      currentClip match
        case Some(newClip) =>
          println(s"🎬 Passaggio a clip successiva: ${newClip.sourceUrl}")
          syncVideoPreview()
        case None =>
          view.updatePreview(None, 0.0, false)
  }

  view.onTogglePlaybackRequested = { () =>
    currentPlayerState = currentPlayerState match
      case Paused     => Playing(speed = 1.0)
      case Playing(_) => Paused
    syncVideoPreview()
  }

  view.onKeyReleased = (event: scalafx.scene.input.KeyEvent) => inputHandler.handleKeyEvent(event)

  def viewComponent: VBox = view

  view.render(currentTimeline)
  syncVideoPreview()