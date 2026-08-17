package app.view.components

import scalafx.scene.layout.StackPane
import scalafx.scene.media.{Media, MediaPlayer, MediaView}
import scalafx.geometry.Pos
import scalafx.application.Platform

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

  private var activeJfxPlayer: Option[javafx.scene.media.MediaPlayer] = None
  private var currentLoadedUrl: Option[String] = None
  private var isSeekingOrLoading: Boolean = false

  def update(state: PlaybackState, onTimeUpdated: Double => Unit): Unit =
    Platform.runLater:
      state.mediaUrl match
        case Some(url) =>
          val targetTime = javafx.util.Duration.seconds(state.relativeTime)

          if !currentLoadedUrl.contains(url) then
            isSeekingOrLoading = true

            activeJfxPlayer.foreach: p =>
              p.setOnEndOfMedia(null)
              p.stop()
              p.dispose()
            activeJfxPlayer = None
            jfxMediaView.setMediaPlayer(null)
            currentLoadedUrl = Some(url)

            try
              val jfxMedia = new javafx.scene.media.Media(url)
              val jfxPlayer = new javafx.scene.media.MediaPlayer(jfxMedia)
              jfxPlayer.setMute(true)

              jfxMediaView.setMediaPlayer(jfxPlayer)
              activeJfxPlayer = Some(jfxPlayer)

              jfxPlayer.currentTimeProperty().addListener: (_, _, newTime) =>
                if !isSeekingOrLoading && jfxPlayer.getStatus == javafx.scene.media.MediaPlayer.Status.PLAYING then
                  onTimeUpdated(newTime.toSeconds)

              jfxPlayer.setOnReady(() =>
                jfxPlayer.seek(targetTime)
                Platform.runLater:
                  isSeekingOrLoading = false
                  if state.isPlaying then jfxPlayer.play()
                  else jfxPlayer.pause()
              )

              jfxPlayer.setOnEndOfMedia(() =>
                val duration = jfxMedia.getDuration
                if duration != null && !duration.isUnknown then
                  onTimeUpdated(duration.toSeconds)
              )
            catch
              case _: Exception =>
                isSeekingOrLoading = false

          else
            activeJfxPlayer.foreach: player =>
              if !isSeekingOrLoading then
                val status = player.getStatus

                if state.isPlaying then
                  if status != javafx.scene.media.MediaPlayer.Status.PLAYING then player.play()
                else
                  if status == javafx.scene.media.MediaPlayer.Status.PLAYING then player.pause()

                val diff = Math.abs(player.getCurrentTime.toSeconds - state.relativeTime)
                if diff > 0.2 then
                  player.seek(targetTime)

        case None =>
          isSeekingOrLoading = false
          activeJfxPlayer.foreach: p =>
            p.stop()
            p.dispose()
          activeJfxPlayer = None
          currentLoadedUrl = None
          jfxMediaView.setMediaPlayer(null)