package app.controller

import scalafx.Includes.*
import core.model.*
import core.engine.*
import app.view.TimelineView
import scalafx.scene.layout.VBox
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

  private val view = new TimelineView()
  private val inputHandler = new InputHandler(onTogglePlayback = view.onTogglePlaybackRequested)

  // Calcola quale clip è attiva sotto il cursore
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

  // --- CALLBACK DELLA VIEW ---

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
      case None =>
        println("🟡 Selezione annullata.")
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

  // Quando l'utente trascina manualmente lo slider
  view.onTimeChanged = { newCursorTime =>
    currentTime = newCursorTime
    syncVideoPreview()
  }

  // Quando il video si muove, aggiorna la posizione corrente nel controller
  view.onVideoTimeUpdated = { newVideoTime =>
    getActiveClip() match
      case Some(clip) =>
        currentTime = clip.startTime + newVideoTime - clip.trimStart
        view.updateTimelineTime(currentTime)
      case None => ()
  }

  view.onTogglePlaybackRequested = { () =>
    currentPlayerState = currentPlayerState match
      case Paused =>
        Playing(speed = 1.0)
      case Playing(_) =>
        Paused
    syncVideoPreview()
  }

  view.onKeyReleased = (event: scalafx.scene.input.KeyEvent) => inputHandler.handleKeyEvent(event)

  def viewComponent: VBox = view

  view.render(currentTimeline)
  syncVideoPreview()