package app

import controller.TimelineController
import scalafx.application.JFXApp3
import scalafx.scene.Scene


object MainApp extends JFXApp3:

  override def start(): Unit =
    val controller = new TimelineController()

    stage = new JFXApp3.PrimaryStage {
      title = "Video Editor Timeline"
      maximized = true
      width = 650
      height = 320
      scene = new Scene {
        root = controller.view
      }
    }