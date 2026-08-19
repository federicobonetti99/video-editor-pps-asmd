package app.view.components

import scalafx.scene.media.{Media, MediaPlayer}
import scalafx.util.Duration
import java.util.concurrent.atomic.AtomicReference

class AudioPlayer:

  private val currentPlayer = new AtomicReference[Option[MediaPlayer]](None)
  private val currentUrl = new AtomicReference[Option[String]](None)

  def update(audioUrlOpt: Option[String], relativeTimeSeconds: Double, isPlaying: Boolean): Unit =
    (audioUrlOpt, currentUrl.get()) match
      case (None, _) =>
        stopAndDispose()

      case (Some(newUrl), current) if !current.contains(newUrl) =>
        stopAndDispose()
        try
          val media = new Media(newUrl)
          val player = new MediaPlayer(media)
          player.startTime = Duration(0.0)

          player.onReady = () =>
            player.seek(Duration(relativeTimeSeconds * 1000.0))
            if isPlaying then player.play() else player.pause()

          currentPlayer.set(Some(player))
          currentUrl.set(Some(newUrl))
        catch
          case _: Exception => stopAndDispose()

      case (Some(_), Some(_)) =>
        currentPlayer.get().foreach: player =>
          val status = player.status.value
          val targetDuration = Duration(relativeTimeSeconds * 1000.0)

          if isPlaying then
            if status != MediaPlayer.Status.Playing.delegate then
              player.seek(targetDuration)
              player.play()
          else
            if status == MediaPlayer.Status.Playing.delegate then
              player.pause()
            player.seek(targetDuration)

  def stop(): Unit =
    stopAndDispose()

  private def stopAndDispose(): Unit =
    currentPlayer.get().foreach: player =>
      player.stop()
      player.dispose()
    currentPlayer.set(None)
    currentUrl.set(None)