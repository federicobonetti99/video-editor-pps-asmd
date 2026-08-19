package app.view.components

import scalafx.scene.layout.StackPane
import scalafx.scene.media.MediaView
import scalafx.geometry.Pos
import scalafx.application.Platform
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

case class PlaybackState(
                          mediaUrl: Option[String],
                          relativeTime: Double,
                          isPlaying: Boolean
                        )

class VideoPreview(width: Double, height: Double) extends StackPane:

  alignment = Pos.Center
  prefWidth = width
  prefHeight = height
  minHeight = height
  maxHeight = height
  style = "-fx-background-color: black; -fx-border-color: #333333; -fx-border-width: 2px;"

  private val jfxMediaView = new javafx.scene.media.MediaView()
  private val mediaView = new MediaView(jfxMediaView):
    fitWidth = width
    fitHeight = height
    preserveRatio = true

  children = Seq(mediaView)

  private val activeJfxPlayer = new AtomicReference[Option[javafx.scene.media.MediaPlayer]](None)
  private val currentLoadedUrl = new AtomicReference[Option[String]](None)
  private val isSeekingOrLoading = new AtomicBoolean(false)

  def update(state: PlaybackState, onTimeUpdated: Double => Unit): Unit =
    Platform.runLater:
      state.mediaUrl match
        case Some(url) =>
          val targetDuration = javafx.util.Duration.seconds(state.relativeTime)

          if !currentLoadedUrl.get().contains(url) then
            isSeekingOrLoading.set(true)

            activeJfxPlayer.get().foreach: p =>
              p.setOnEndOfMedia(null)
              p.stop()
              p.dispose()
            activeJfxPlayer.set(None)
            jfxMediaView.setMediaPlayer(null)
            currentLoadedUrl.set(Some(url))

            try
              val jfxMedia = new javafx.scene.media.Media(url)
              val jfxPlayer = new javafx.scene.media.MediaPlayer(jfxMedia)

              jfxPlayer.setMute(true)
              jfxPlayer.setVolume(0.0)

              jfxMediaView.setMediaPlayer(jfxPlayer)
              activeJfxPlayer.set(Some(jfxPlayer))

              jfxPlayer.currentTimeProperty().addListener: (_, _, newTime) =>
                if !isSeekingOrLoading.get() && jfxPlayer.getStatus == javafx.scene.media.MediaPlayer.Status.PLAYING then
                  onTimeUpdated(newTime.toSeconds)

              jfxPlayer.setOnReady: () =>
                jfxPlayer.seek(targetDuration)
                Platform.runLater:
                  isSeekingOrLoading.set(false)
                  if state.isPlaying then jfxPlayer.play()
                  else jfxPlayer.pause()

              jfxPlayer.setOnEndOfMedia: () =>
                val duration = jfxMedia.getDuration
                if duration != null && !duration.isUnknown then
                  onTimeUpdated(duration.toSeconds)
            catch
              case _: Exception =>
                isSeekingOrLoading.set(false)

          else
            activeJfxPlayer.get().foreach: player =>
              if !isSeekingOrLoading.get() then
                val status = player.getStatus
                val currentSec = player.getCurrentTime.toSeconds
                val diff = Math.abs(currentSec - state.relativeTime)

                if state.isPlaying then
                  if status != javafx.scene.media.MediaPlayer.Status.PLAYING then
                    if diff > 0.1 then player.seek(targetDuration)
                    player.play()
                  else if diff > 0.4 then
                    player.seek(targetDuration)
                else
                  if status == javafx.scene.media.MediaPlayer.Status.PLAYING then
                    player.pause()
                  if diff > 0.03 then
                    player.seek(targetDuration)

        case None =>
          isSeekingOrLoading.set(false)
          activeJfxPlayer.get().foreach: p =>
            p.stop()
            p.dispose()
          activeJfxPlayer.set(None)
          currentLoadedUrl.set(None)
          jfxMediaView.setMediaPlayer(null)