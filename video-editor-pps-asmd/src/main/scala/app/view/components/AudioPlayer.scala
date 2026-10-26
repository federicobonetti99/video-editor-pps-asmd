package app.view.components

import org.bytedeco.javacv.{FFmpegFrameGrabber, FFmpegFrameRecorder}
import scalafx.scene.media.{Media, MediaPlayer}
import scalafx.util.Duration

import java.io.File
import java.net.URI
import java.nio.file.Files
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

case class ActiveAudioTrackInfo(
                                 sourceUrl: String,
                                 relativeTimeSeconds: Double,
                                 volume: Double = 1.0
                               )

class AudioPlayer:

  private val activePlayers = new AtomicReference[Map[String, MediaPlayer]](Map.empty)
  private val extractedAudioCache = new ConcurrentHashMap[String, String]()

  private val videoExtensions = Set("mp4", "mkv", "avi", "mov", "flv", "webm")

  private def isVideoUrl(url: String): Boolean =
    val clean = url.toLowerCase.takeWhile(_ != '?')
    videoExtensions.exists(ext => clean.endsWith(s".$ext"))

  private def extractLocalPath(url: String): String =
    try
      val uri = new URI(url)
      if uri.getScheme == "file" then
        val p = uri.getPath
        if p.startsWith("/") && p.length > 2 && p.charAt(2) == ':' then p.substring(1) else p
      else url
    catch
      case _: Exception => url

  private def resolveAudioUrl(sourceUrl: String): Option[String] =
    if !isVideoUrl(sourceUrl) then
      Some(sourceUrl)
    else
      Option(extractedAudioCache.computeIfAbsent(sourceUrl, _ => {
        try
          val inputPath = extractLocalPath(sourceUrl)
          val tempWav = Files.createTempFile("video_audio_", ".wav").toFile
          tempWav.deleteOnExit()

          val grabber = new FFmpegFrameGrabber(inputPath)
          grabber.start()

          if grabber.getAudioChannels <= 0 then
            grabber.stop()
            grabber.release()
            null
          else
            val recorder = new FFmpegFrameRecorder(tempWav, grabber.getAudioChannels)
            recorder.setAudioCodecName("pcm_s16le")
            recorder.setSampleRate(grabber.getSampleRate)
            recorder.start()

            @tailrec
            def recordSamples(): Unit =
              Option(grabber.grabSamples()) match
                case Some(frame) =>
                  recorder.record(frame)
                  recordSamples()
                case None => ()

            recordSamples()

            recorder.stop()
            recorder.release()
            grabber.stop()
            grabber.release()

            tempWav.toURI.toString
        catch
          case _: Exception => null
      }))

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
      val targetVolume = Math.max(0.0, Math.min(1.0, info.volume))
      val targetDuration = Duration(Math.max(0.0, info.relativeTimeSeconds) * 1000.0)

      acc.get(info.sourceUrl) match
        case Some(existingPlayer) =>
          existingPlayer.volume = targetVolume
          if isPlaying then
            if existingPlayer.status.value != MediaPlayer.Status.Playing.delegate then
              existingPlayer.seek(targetDuration)
              existingPlayer.play()
            else
              val currentTimeSec = existingPlayer.currentTime.value.toSeconds
              if Math.abs(currentTimeSec - info.relativeTimeSeconds) > 0.3 then
                existingPlayer.seek(targetDuration)
          else
            existingPlayer.pause()
            existingPlayer.seek(targetDuration)
          acc

        case None =>
          resolveAudioUrl(info.sourceUrl) match
            case Some(resolvedUrl) =>
              try
                val media = new Media(resolvedUrl)
                val newPlayer = new MediaPlayer(media)
                newPlayer.volume = targetVolume
                newPlayer.startTime = Duration.Zero
                newPlayer.seek(targetDuration)
                if isPlaying then newPlayer.play() else newPlayer.pause()
                acc + (info.sourceUrl -> newPlayer)
              catch
                case _: Throwable => acc
            case None =>
              acc

    activePlayers.set(updatedMap)

  def stopAll(): Unit =
    val currentMap = activePlayers.getAndSet(Map.empty)
    currentMap.values.foreach: player =>
      try
        player.stop()
        player.dispose()
      catch
        case _: Throwable => ()