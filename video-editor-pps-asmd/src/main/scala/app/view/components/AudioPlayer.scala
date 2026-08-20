package app.view.components

import scalafx.scene.media.{Media, MediaPlayer}
import scalafx.util.Duration
import java.util.concurrent.atomic.AtomicReference

case class ActiveAudioTrackInfo(
                                 sourceUrl: String,
                                 relativeTimeSeconds: Double
                               )

class AudioPlayer:

  private val activePlayers = new AtomicReference[Map[String, MediaPlayer]](Map.empty)

  def update(activeAudios: List[ActiveAudioTrackInfo], isPlaying: Boolean): Unit =
    val currentMap = activePlayers.get()
    val activeUrls = activeAudios.map(_.sourceUrl).toSet

    val (stillActiveMap, removedMap) = currentMap.partition((url, _) => activeUrls.contains(url))
    removedMap.values.foreach: player =>
      try
        player.stop()
        player.dispose()
      catch
        case _: Throwable => ()

    val updatedMap = activeAudios.foldLeft(stillActiveMap): (acc, info) =>
      acc.get(info.sourceUrl) match
        case Some(existingPlayer) =>
          if isPlaying then
            if existingPlayer.status.value != MediaPlayer.Status.Playing.delegate then
              existingPlayer.seek(Duration(info.relativeTimeSeconds * 1000.0))
              existingPlayer.play()
            else
              val currentTimeSec = existingPlayer.currentTime.value.toSeconds
              if Math.abs(currentTimeSec - info.relativeTimeSeconds) > 0.3 then
                existingPlayer.seek(Duration(info.relativeTimeSeconds * 1000.0))
          else
            existingPlayer.pause()
            existingPlayer.seek(Duration(info.relativeTimeSeconds * 1000.0))
          acc

        case None =>
          try
            val media = new Media(info.sourceUrl)
            val newPlayer = new MediaPlayer(media)
            newPlayer.startTime = Duration.Zero
            newPlayer.seek(Duration(info.relativeTimeSeconds * 1000.0))
            if isPlaying then newPlayer.play() else newPlayer.pause()
            acc + (info.sourceUrl -> newPlayer)
          catch
            case _: Throwable => acc

    activePlayers.set(updatedMap)

  def stopAll(): Unit =
    val currentMap = activePlayers.getAndSet(Map.empty)
    currentMap.values.foreach: player =>
      try
        player.stop()
        player.dispose()
      catch
        case _: Throwable => ()