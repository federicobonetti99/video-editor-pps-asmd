package app.controller

import core.model.*
import core.engine.*
import app.view.TimelineView
import scalafx.scene.layout.VBox
import scalafx.animation.AnimationTimer

class TimelineController:

  private val sampleClip = VideoClip(
    sourceUrl = "video1.mp4",
    sourceLength = 10.0,
    startTime = 0.0,
    trimStart = 0.0,
    duration = 10.0,
    effect = VideoEffect.None
  )

  private val maxDuration = 60.0
  private val initialTrack = VideoTrack(id = 1, clips = Nil)
  private var currentTimeline = Timeline(videoTracks = List(initialTrack), audioTracks = Nil)

  private var currentTime: Double = 0.0
  private var currentPlayerState: PlayerState = Paused

  private val view = new TimelineView()

  view.onAddRequested = { cursorTime =>
    val clipAtCursor = sampleClip.copy(startTime = cursorTime)
    currentTimeline = TimelineEngine.addVideoClip(currentTimeline, 1, clipAtCursor)
    view.render(currentTimeline)
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
  }

  view.onSnapRequested = { () =>
    currentTimeline = TimelineEngine.snapClipsTogether(currentTimeline, 1)
    view.render(currentTimeline)
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
    println(s"Player state changed to: $currentPlayerState")
  }

  private var lastTimeNano: Long = 0L

  private val timer: AnimationTimer = AnimationTimer { currentNano =>
    if lastTimeNano == 0L then
      lastTimeNano = currentNano
    else
      val deltaTime = (currentNano - lastTimeNano) / 1e9
      val nextTime = TimelineEngine.updatePlaybackTime(currentTime, currentPlayerState, deltaTime, maxDuration)

      if nextTime != currentTime then
        currentTime = nextTime
        view.updateTimelineTime(currentTime)

      if currentTime >= maxDuration then
        currentPlayerState = Paused
        timer.stop()

      lastTimeNano = currentNano
  }

  view.onTimeChanged = { newCursorTime =>
    currentTime = newCursorTime
  }

  def viewComponent: VBox = view

  view.render(currentTimeline)