package app.view.components

import scalafx.scene.layout.StackPane
import scalafx.scene.media.{Media, MediaPlayer, MediaView}
import scalafx.geometry.Pos
import scalafx.application.Platform

class VideoPreview(width: Double, height: Double) extends StackPane:

  alignment = Pos.Center
  prefWidth = width
  prefHeight = height
  minHeight = height
  maxHeight = height
  style = "-fx-background-color: black; -fx-border-color: #333333; -fx-border-width: 2px;"

  private val jfxMediaView = new javafx.scene.media.MediaView()
  private val mediaView = new MediaView(jfxMediaView) {
    fitWidth = width
    fitHeight = height
    preserveRatio = true
  }

  children = Seq(mediaView)

  private var activeJfxPlayer: Option[javafx.scene.media.MediaPlayer] = None
  private var currentLoadedUrl: Option[String] = None
  private var lastRequestedPlayingState: Option[Boolean] = None

  def update(videoUrlOpt: Option[String], relativeTimeSeconds: Double, isPlaying: Boolean, onTimeUpdated: Double => Unit): Unit =
    Platform.runLater {
      videoUrlOpt match
        case Some(url) =>
          if !currentLoadedUrl.contains(url) then
            activeJfxPlayer.foreach { p =>
              p.stop()
              p.dispose()
            }
            activeJfxPlayer = None
            lastRequestedPlayingState = None

            try {
              val jfxMedia = new javafx.scene.media.Media(url)
              val jfxPlayer = new javafx.scene.media.MediaPlayer(jfxMedia)

              jfxMediaView.setMediaPlayer(jfxPlayer)
              activeJfxPlayer = Some(jfxPlayer)
              currentLoadedUrl = Some(url)

              jfxPlayer.setOnReady(() => {
                jfxPlayer.seek(javafx.util.Duration.millis(relativeTimeSeconds * 1000.0))
              })

              jfxPlayer.currentTimeProperty().addListener { (_, _, newTime) =>
                if jfxPlayer.getStatus == javafx.scene.media.MediaPlayer.Status.PLAYING then
                  onTimeUpdated(newTime.toSeconds)
              }

            } catch {
              case e: Exception => println(s"❌ Errore caricamento video: ${e.getMessage}")
            }

          activeJfxPlayer.foreach {
            player =>
            val targetTime = javafx.util.Duration.millis(relativeTimeSeconds * 1000.0)

            if isPlaying then
              if !lastRequestedPlayingState.contains(true) then
                lastRequestedPlayingState = Some(true)
              player.seek(targetTime)
              player.play()

            else
              if !lastRequestedPlayingState.contains(false) then
                lastRequestedPlayingState = Some(false)
                player.pause()
                player.seek(targetTime)

              val diff = Math.abs(player.getCurrentTime.toSeconds - relativeTimeSeconds)
              if diff > 0.15 then
                player.seek(targetTime)
          }

        case None =>
          activeJfxPlayer.foreach { player =>
            if !lastRequestedPlayingState.contains(false) then
              lastRequestedPlayingState = Some(false)
              player.pause()
            player.seek(javafx.util.Duration.ZERO)
          }
    }