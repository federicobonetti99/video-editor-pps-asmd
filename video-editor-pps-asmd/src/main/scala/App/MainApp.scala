package App

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import Core.Model.*
import Core.Engine.TimelineEngine
import App.View.TimelineView

object MainApp extends JFXApp3:
//EXAMPLE MAIN IN THIS VERSION FOR TESTS
  override def start(): Unit =
    val initialTrack = VideoTrack(id = 1, clips = Nil)
    var currentTimeline = Timeline(videoTracks = List(initialTrack), audioTracks = Nil)

    lazy val timelineView: TimelineView = new TimelineView(
      onAddRequested = () => {
        println("=== DEBUG CLICK ADD ===")
        val newClip = VideoClip("gatto.mp4", 10.0, 0.0, 0.0, 10.0, VideoEffect.None)
        currentTimeline = TimelineEngine.addVideoClip(currentTimeline, 1, newClip)

        currentTimeline.videoTracks.foreach { t =>
          println(s"Traccia ID: ${t.id} | Numero Clip dentro: ${t.clips.size}")
          t.clips.foreach { c =>
            println(s"  -> Clip: ${c.sourceUrl} | Start: ${c.startTime} | Duration: ${c.duration}")
          }
        }
        println("=======================")

        timelineView.render(currentTimeline)
      },
      onCutRequested = () => {
        println("Pulsante CUT cliccato!")
        currentTimeline = TimelineEngine.cutVideoClip(currentTimeline, 1, 0, 3.0)
        timelineView.render(currentTimeline)
      },
      onSnapRequested = () => {
        println("Pulsante SNAP cliccato!")
        currentTimeline = TimelineEngine.snapClipsTogether(currentTimeline, 1)
        timelineView.render(currentTimeline)
      }
    )

    timelineView.render(currentTimeline)

    stage = new JFXApp3.PrimaryStage {
      title = "Video Editor - Spizzico di View"
      width = 620
      height = 250
      scene = new Scene {
        root = timelineView
      }
    }