package app.controller

import scalafx.Includes.*
import core.model.*
import core.engine.*
import app.view.TimelineView
import scalafx.scene.layout.VBox
import scalafx.animation.AnimationTimer
import java.io.File

class TimelineController:

  private val sampleClip = VideoClip(
    sourceUrl = "video1.mp4",
    sourceLength = 10.0,
    startTime = 0.0,
    trimStart = 0.0,
    duration = 10.0,
    effect = VideoEffect.None
  )

  private val initialTrack = VideoTrack(id = 1, clips = Nil)
  private var currentTimeline = Timeline(videoTracks = List(initialTrack), audioTracks = Nil)

  private var currentTime: Double = 0.0
  private var currentPlayerState: PlayerState = Paused
  private var lastTimeNano: Long = 0L

  private val view = new TimelineView()
  private val inputHandler = new InputHandler(onTogglePlayback = view.onTogglePlaybackRequested)

  private def syncVideoPreview(): Unit =
    val activeClipOpt = currentTimeline.videoTracks
      .flatMap(_.clips)
      .find(c => currentTime >= c.startTime && currentTime < (c.startTime + c.duration))

    activeClipOpt match
      case Some(clip) =>
        val relativeTime = (currentTime - clip.startTime) + clip.trimStart
        val isPlaying = currentPlayerState match
          case Playing(_) => true
          case Paused => false
        view.updatePreview(Some(clip.sourceUrl), relativeTime, isPlaying)
      case None =>
        view.updatePreview(None, 0.0, false)

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

        currentTimeline = TimelineEngine.addVideoClip(currentTimeline, 1, importedClip)

        view.render(currentTimeline)
        syncVideoPreview()
        println(s"🟢 Importato: ${file.getName} | Durata: $durataReale secondi")

      case None =>
        println("🟡 Selezione del file annullata.")
  }

  view.onAddRequested = { cursorTime =>
    val clipAtCursor = sampleClip.copy(startTime = cursorTime)
    currentTimeline = TimelineEngine.addVideoClip(currentTimeline, 1, clipAtCursor)
    view.render(currentTimeline)
    syncVideoPreview()
  }

  view.onCutRequested = { cursorTime =>
    val track = currentTimeline.videoTracks.find(_.id == 1).get
    val clipIndexOpt = track.clips.indexWhere { c =>
      cursorTime >= c.startTime && cursorTime < (c.startTime + c.duration)
    }

    if clipIndexOpt != -1 then
      val targetClip = track.clips(clipIndexOpt)
      val relativeCut = cursorTime - targetClip.startTime
      currentTimeline = TimelineEngine.cutVideoClip(currentTimeline, 1, clipIndexOpt, relativeCut)
      view.render(currentTimeline)
      syncVideoPreview()
  }

  view.onSnapRequested = { () =>
    currentTimeline = TimelineEngine.snapClipsTogether(currentTimeline, 1)
    view.render(currentTimeline)
    syncVideoPreview()
  }

  view.onTimeChanged = { newCursorTime =>
    currentTime = newCursorTime
    syncVideoPreview()
  }

  view.onTogglePlaybackRequested = { () =>
    currentPlayerState = currentPlayerState match
      case Paused =>
        lastTimeNano = 0L
        timer.start()
        Playing(speed = 1.0)
      case Playing(_) =>
        timer.stop()
        Paused
    syncVideoPreview()
    println(s"Player state changed to: $currentPlayerState")
  }

  view.onKeyReleased = (event: scalafx.scene.input.KeyEvent) => inputHandler.handleKeyEvent(event)

  private val timer: AnimationTimer = AnimationTimer { currentNano =>
    if lastTimeNano == 0L then
      lastTimeNano = currentNano
    else
      val deltaTime = (currentNano - lastTimeNano) / 1e9
      val limit = 60.0

      val nextTime = TimelineEngine.updatePlaybackTime(currentTime, currentPlayerState, deltaTime, limit)

      if nextTime != currentTime then
        currentTime = nextTime
        view.updateTimelineTime(currentTime)
        syncVideoPreview()

      if currentTime >= limit then
        currentPlayerState = Paused
        timer.stop()
        syncVideoPreview()

      lastTimeNano = currentNano
  }

  def viewComponent: VBox = view

  view.render(currentTimeline)
  syncVideoPreview()