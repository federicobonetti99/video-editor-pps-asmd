package app.controller

import scalafx.Includes.*
import core.model.*
import core.engine.*
import app.view.TimelineView
import scalafx.scene.layout.VBox

class TimelineController:

  private val initialTrack = VideoTrack(id = 1, clips = Nil)
  private var currentTimeline = Timeline(videoTracks = List(initialTrack), audioTracks = Nil)

  private var currentTime: Double = 0.0
  private var currentPlayerState: PlayerState = Paused

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

  view.onDeleteRequested = { () =>
    val track = currentTimeline.videoTracks.find(_.id == 1).get
    val clipIndexOpt = track.clips.indexWhere { c =>
      currentTime >= c.startTime && currentTime < (c.startTime + c.duration)
    }

    if clipIndexOpt != -1 then
      println(s"🗑️ Eliminazione della clip all'indice: $clipIndexOpt")
      currentTimeline = TimelineEngine.removeVideoClip(currentTimeline, trackId = 1, clipIndex = clipIndexOpt)
      view.render(currentTimeline)
      syncVideoPreview()
    else
      println("⚠️ Nessuna clip sotto il cursore da eliminare.")
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

          println(s"🎬 Taglio rilevato! Passaggio a: ${newClip.sourceUrl} al secondo: $relativeTime")

          val isPlaying = currentPlayerState match
            case Playing(_) => true
            case Paused => false

          view.updatePreview(Some(newClip.sourceUrl), relativeTime, isPlaying)

        case None =>
          view.updatePreview(None, 0.0, false)
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