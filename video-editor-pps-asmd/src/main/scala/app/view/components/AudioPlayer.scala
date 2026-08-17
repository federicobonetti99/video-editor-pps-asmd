package app.view.components

import scalafx.application.Platform

class AudioPlayer:

  private var activeJfxPlayer: Option[javafx.scene.media.MediaPlayer] = None
  private var currentLoadedUrl: Option[String] = None
  private var isSeekingOrLoading: Boolean = false

  def update(audioUrlOpt: Option[String], relativeTimeSeconds: Double, isPlaying: Boolean): Unit =
    Platform.runLater {
      audioUrlOpt match
        case Some(url) =>
          val targetTime = javafx.util.Duration.seconds(relativeTimeSeconds)

          if !currentLoadedUrl.contains(url) then
            isSeekingOrLoading = true

            activeJfxPlayer.foreach { p =>
              p.stop()
              p.dispose()
            }
            activeJfxPlayer = None
            currentLoadedUrl = Some(url)

            try {
              val jfxMedia = new javafx.scene.media.Media(url)
              val jfxPlayer = new javafx.scene.media.MediaPlayer(jfxMedia)
              activeJfxPlayer = Some(jfxPlayer)

              jfxPlayer.setOnReady(() => {
                jfxPlayer.seek(targetTime)
                Platform.runLater {
                  isSeekingOrLoading = false
                  if isPlaying then jfxPlayer.play()
                  else jfxPlayer.pause()
                }
              })

            } catch {
              case e: Exception =>
                isSeekingOrLoading = false
            }

          else
            activeJfxPlayer.foreach { player =>
              if !isSeekingOrLoading then
                val status = player.getStatus

                if isPlaying then
                  if status != javafx.scene.media.MediaPlayer.Status.PLAYING then player.play()
                  else
                    if status == javafx.scene.media.MediaPlayer.Status.PLAYING then player.pause()

                val diff = Math.abs(player.getCurrentTime.toSeconds - relativeTimeSeconds)
                if diff > 0.2 then
                  player.seek(targetTime)
            }

        case None =>
          isSeekingOrLoading = false
          activeJfxPlayer.foreach { p =>
            p.stop()
            p.dispose()
          }
          activeJfxPlayer = None
          currentLoadedUrl = None
    }