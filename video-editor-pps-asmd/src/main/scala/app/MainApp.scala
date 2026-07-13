package app

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import app.controller.TimelineController

object MainApp extends JFXApp3:

  override def start(): Unit =
    val controller = new TimelineController()

    stage = new JFXApp3.PrimaryStage {
      title = "Video Editor Timeline"
      width = 650
      height = 320
      scene = new Scene {
        root = controller.viewComponent
      }
    }