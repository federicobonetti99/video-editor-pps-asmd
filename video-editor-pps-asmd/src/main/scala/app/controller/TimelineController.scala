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

  private def totalTimelineDuration: Double =
    currentTimeline.videoTracks.flatMap(_.clips).map(c => c.startTime + c.duration).maxOption.getOrElse(0.0)

  private val inputHandler = new InputHandler(onTogglePlayback = () => {
    if currentTime >= totalTimelineDuration && totalTimelineDuration > 0 then
      currentTime = 0.0
      view.updateTimelineTime(0.0)

    currentPlayerState = currentPlayerState match
      case Paused     => Playing(speed = 1.0)
      case Playing(_) => Paused
    syncVideoPreview()
  })

  view.onClipSelected = { clipOpt =>
    clipOpt match
      case Some(SelectedClip.SelectedVideo(clip)) => println(s"🎯 Video selezionato: ${clip.sourceUrl}")
      case Some(SelectedClip.SelectedAudio(clip)) => println(s"🎯 Audio selezionato: ${clip.sourceUrl}")
      case None                                   => println("⚪ Nessuna clip selezionata.")
  }

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
    view.getSelectedClip match
      case Some(SelectedClip.SelectedVideo(selVideo)) =>
        val videoTrack = currentTimeline.videoTracks.find(_.id == 1).get
        val idx = videoTrack.clips.indexWhere(c => c.sourceUrl == selVideo.sourceUrl && Math.abs(c.startTime - selVideo.startTime) < 0.001)
        if idx != -1 then
          println(s"🗑️ Eliminazione della sola VideoClip all'indice: $idx")
          currentTimeline = TimelineEngine.removeVideoClip(currentTimeline, trackId = 1, clipIndex = idx)

      case Some(SelectedClip.SelectedAudio(selAudio)) =>
        val audioTrack = currentTimeline.audioTracks.find(_.id == 1).get
        val idx = audioTrack.clips.indexWhere(c => c.sourceUrl == selAudio.sourceUrl && Math.abs(c.startTime - selAudio.startTime) < 0.001)
        if idx != -1 then
          println(s"🗑️ Eliminazione della sola AudioClip all'indice: $idx")
          currentTimeline = TimelineEngine.removeAudioClip(currentTimeline, trackId = 1, clipIndex = idx)

      case None =>
        val videoTrack = currentTimeline.videoTracks.find(_.id == 1).get
        val clipIndexOpt = videoTrack.clips.indexWhere { c =>
          currentTime >= c.startTime && currentTime < (c.startTime + c.duration)
        }
        if clipIndexOpt != -1 then
          println(s"🗑️ Eliminazione video + audio sotto il cursore all'indice: $clipIndexOpt")
          var newTimeline = TimelineEngine.removeVideoClip(currentTimeline, trackId = 1, clipIndex = clipIndexOpt)
          newTimeline = TimelineEngine.removeAudioClip(newTimeline, trackId = 1, clipIndex = clipIndexOpt)
          currentTimeline = newTimeline
        else
          println("⚠️ Nessuna clip sotto il cursore da eliminare.")

    view.selectClip(None)
    view.render(currentTimeline)
    syncVideoPreview()
  }

  view.onCutRequested = { cursorTime =>
    val videoTrack = currentTimeline.videoTracks.find(_.id == 1).get
    val audioTrack = currentTimeline.audioTracks.find(_.id == 1).get

    currentTimeline = view.getSelectedClip match
      case Some(SelectedClip.SelectedVideo(selVideo)) =>
        val idx = videoTrack.clips.indexWhere(c => c.sourceUrl == selVideo.sourceUrl && Math.abs(c.startTime - selVideo.startTime) < 0.001)
        val targetClip = videoTrack.clips.lift(idx)
        targetClip.filter(c => cursorTime > c.startTime && cursorTime < (c.startTime + c.duration))
          .fold(currentTimeline)(c => TimelineEngine.cutVideoClip(currentTimeline, 1, idx, cursorTime - c.startTime))

      case Some(SelectedClip.SelectedAudio(selAudio)) =>
        val idx = audioTrack.clips.indexWhere(c => c.sourceUrl == selAudio.sourceUrl && Math.abs(c.startTime - selAudio.startTime) < 0.001)
        val targetClip = audioTrack.clips.lift(idx)
        targetClip.filter(c => cursorTime > c.startTime && cursorTime < (c.startTime + c.duration))
          .fold(currentTimeline)(c => TimelineEngine.cutAudioClip(currentTimeline, 1, idx, cursorTime - c.startTime))

      case None =>
        val videoIdx = videoTrack.clips.indexWhere(c => cursorTime >= c.startTime && cursorTime < (c.startTime + c.duration))
        val audioIdx = audioTrack.clips.indexWhere(c => cursorTime >= c.startTime && cursorTime < (c.startTime + c.duration))

        val timelineWithCutVideo = if videoIdx != -1 then
          val relativeCut = cursorTime - videoTrack.clips(videoIdx).startTime
          TimelineEngine.cutVideoClip(currentTimeline, 1, videoIdx, relativeCut)
        else currentTimeline

        if audioIdx != -1 then
          val relativeCut = cursorTime - audioTrack.clips(audioIdx).startTime
          TimelineEngine.cutAudioClip(timelineWithCutVideo, 1, audioIdx, relativeCut)
        else timelineWithCutVideo

    view.render(currentTimeline)
    syncVideoPreview()
  }

  view.onSnapRequested = { () =>
    view.getSelectedClip match
      case Some(SelectedClip.SelectedVideo(_)) =>
        currentTimeline = TimelineEngine.snapClipsTogether(currentTimeline, 1)
      case Some(SelectedClip.SelectedAudio(_)) =>
        currentTimeline = TimelineEngine.snapClipsTogether(currentTimeline, 1)
      case None =>
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
    if currentTime >= totalTimelineDuration && totalTimelineDuration > 0 then
      currentTime = 0.0
      view.updateTimelineTime(0.0)

    currentPlayerState = currentPlayerState match
      case Paused     => Playing(speed = 1.0)
      case Playing(_) => Paused
    syncVideoPreview()
  }

  view.onKeyReleased = (event: scalafx.scene.input.KeyEvent) => inputHandler.handleKeyEvent(event)

  def viewComponent: VBox = view

  view.render(currentTimeline)
  syncVideoPreview()