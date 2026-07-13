package app.controller

import core.model.*
import core.engine.*
import app.view.TimelineView
import scalafx.scene.layout.VBox

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

  // 1. Creiamo la View in modo pulito
  private val view = new TimelineView()

  // 2. Registriamo i comportamenti SUBITO DOPO la creazione dell'oggetto
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

  def viewComponent: VBox = view

  // Render iniziale
  view.render(currentTimeline)