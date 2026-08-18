package app.view.components

import scalafx.application.Platform
import java.util.concurrent.atomic.AtomicReference

private case class AudioInternalState(
                                       player: Option[javafx.scene.media.MediaPlayer] = None,
                                       loadedUrl: Option[String] = None,
                                       isSeekingOrLoading: Boolean = false
                                     )

class AudioPlayer:

  private val state = new AtomicReference[AudioInternalState](AudioInternalState())

  def update(audioUrlOpt: Option[String], relativeTimeSeconds: Double, isPlaying: Boolean): Unit =
    Platform.runLater:
      val currentState = state.get()
      audioUrlOpt match
        case Some(url) =>
          val targetTime = javafx.util.Duration.seconds(relativeTimeSeconds)

          if !currentState.loadedUrl.contains(url) then
            currentState.player.foreach: p =>
              p.stop()
              p.dispose()

            state.set(AudioInternalState(loadedUrl = Some(url), isSeekingOrLoading = true))

            try
              val jfxMedia = new javafx.scene.media.Media(url)
              val jfxPlayer = new javafx.scene.media.MediaPlayer(jfxMedia)
              state.set(AudioInternalState(player = Some(jfxPlayer), loadedUrl = Some(url), isSeekingOrLoading = true))

              jfxPlayer.setOnReady(() =>
                jfxPlayer.seek(targetTime)
                Platform.runLater:
                  state.set(AudioInternalState(player = Some(jfxPlayer), loadedUrl = Some(url), isSeekingOrLoading = false))
                  if isPlaying then jfxPlayer.play()
                  else jfxPlayer.pause()
              )
            catch
              case _: Exception =>
                state.set(AudioInternalState(loadedUrl = Some(url), isSeekingOrLoading = false))

          else
            currentState.player.foreach: player =>
              if isPlaying then
                if player.getStatus != javafx.scene.media.MediaPlayer.Status.PLAYING then
                  player.play()
              else
                if player.getStatus == javafx.scene.media.MediaPlayer.Status.PLAYING then
                  player.pause()

              if !currentState.isSeekingOrLoading then
                val diff = Math.abs(player.getCurrentTime.toSeconds - relativeTimeSeconds)
                if diff > 0.2 then
                  player.seek(targetTime)

        case None =>
          currentState.player.foreach: p =>
            p.stop()
            p.dispose()
          state.set(AudioInternalState())