package app.view.components

import scalafx.scene.media.{Media, MediaPlayer}
import core.model.*

class AudioPlayer:

  private var currentPlayer: Option[MediaPlayer] = None
  private var currentSourceUrl: Option[String] = None

  def sync(timeline: Timeline, currentTime: Double, isPlaying: Boolean): Unit =
    val activeAudioClip = timeline.audioTracks
      .flatMap(_.clips)
      .find(c => currentTime >= c.startTime && currentTime < (c.startTime + c.duration))

    activeAudioClip match
      case Some(clip) =>
        val relativeTime = (currentTime - clip.startTime) + clip.trimStart

        if currentSourceUrl != Some(clip.sourceUrl) then
          currentPlayer.foreach(_.stop())
          val player = new MediaPlayer(new Media(clip.sourceUrl))
          currentPlayer = Some(player)
          currentSourceUrl = Some(clip.sourceUrl)

        currentPlayer.foreach { player =>
          val mediaTimeSeconds = player.currentTime.value.toSeconds
          if Math.abs(mediaTimeSeconds - relativeTime) > 0.1 then
            player.seek(scalafx.util.Duration(relativeTime * 1000.0))

          if isPlaying then player.play() else player.pause()
        }

      case None =>
        currentPlayer.foreach(_.stop())
        currentPlayer = None
        currentSourceUrl = None

  def stop(): Unit =
    currentPlayer.foreach(_.stop())
    currentPlayer = None
    currentSourceUrl = None