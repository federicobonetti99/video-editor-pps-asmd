package core.engine

import core.model.*
import java.net.URI
import java.io.File
import java.util.Locale

object ExportCommandBuilder:

  private def fmt(value: Double, decimals: Int = 3): String =
    String.format(Locale.US, s"%.${decimals}f", Double.box(value))

  private def extractFilePath(url: String): String =
    try
      val uri = URI.create(url)
      if uri.getScheme == "file" then new File(uri).getAbsolutePath
      else url.stripPrefix("file:")
    catch
      case _: Throwable => url.stripPrefix("file:").stripPrefix("/")

  private def formatEffect(effect: VideoEffect, clipDuration: Double): String =
    effect match
      case VideoEffect.None => ""
      case VideoEffect.Grayscale => ",hue=s=0"
      case VideoEffect.Sepia => ",colorchannelmixer=.393:.769:.189:0:.349:.686:.168:0:.272:.534:.131"
      case VideoEffect.Invert => ",negate"
      case VideoEffect.FadeIn(duration) =>
        val fadeDur = Math.min(duration, clipDuration)
        s",fade=t=in:st=0:d=${fmt(fadeDur)}"
      case VideoEffect.ZoomIn(zoomFactor) =>
        val z = fmt(zoomFactor, 2)
        s",scale=iw*$z:ih*$z,crop=iw/$z:ih/$z"
      case VideoEffect.Shake(intensity, frequency) =>
        val intVal = fmt(intensity, 0)
        val int2Val = fmt(intensity * 2, 0)
        val freqVal = fmt(frequency, 1)
        s",crop=iw-$int2Val:ih-$int2Val:x='$intVal+$intVal*sin($freqVal*t)':y='$intVal+$intVal*cos($freqVal*t)',scale=iw+$int2Val:ih+$int2Val"

  def buildCommand(timeline: Timeline, settings: ExportSettings): ExportCommand =
    val videoClipsWithTrack = timeline.videoTracks.sortBy(_.id).flatMap(t => t.clips.map(c => (t.id, c)))
    val audioClipsWithTrack = timeline.audioTracks.sortBy(_.id).flatMap(t => t.clips.map(c => (t.id, c)))

    val totalDuration = {
      val maxV = videoClipsWithTrack.map(_._2.endTime).maxOption.getOrElse(0.0)
      val maxA = audioClipsWithTrack.map(_._2.endTime).maxOption.getOrElse(0.0)
      Math.max(1.0, Math.max(maxV, maxA))
    }

    val allUniqueFiles = (videoClipsWithTrack.map(_._2.sourceUrl) ++ audioClipsWithTrack.map(_._2.sourceUrl))
      .distinct
      .map(extractFilePath)

    val fileIndexMap = allUniqueFiles.zipWithIndex.toMap
    val inputArgs = allUniqueFiles.flatMap(file => Seq("-i", file))

    val baseVideoFilter = s"color=c=black:s=${settings.width}x${settings.height}:r=${settings.fps}:d=${fmt(totalDuration)}[bg0]"

    val videoFilterParts = videoClipsWithTrack.zipWithIndex.map { case ((trackId, clip), idx) =>
      val fileIdx = fileIndexMap(extractFilePath(clip.sourceUrl))
      val effectFilter = formatEffect(clip.effect, clip.duration)
      val scaleFilter = s"scale=${settings.width}:${settings.height}:force_original_aspect_ratio=decrease,pad=${settings.width}:${settings.height}:(ow-iw)/2:(oh-ih)/2"
      val trimFilter = s"trim=start=${fmt(clip.trimStart)}:duration=${fmt(clip.duration)},setpts=PTS-STARTPTS+${fmt(clip.startTime)}/TB"

      s"[$fileIdx:v]$trimFilter,$scaleFilter$effectFilter,setsar=1[v$idx]"
    }

    val overlayChains = videoClipsWithTrack.indices.map { idx =>
      val prevBg = if idx == 0 then "[bg0]" else s"[bg$idx]"
      val clip = videoClipsWithTrack(idx)._2
      val outBg = if idx == videoClipsWithTrack.size - 1 then "[vout]" else s"[bg${idx + 1}]"
      val start = fmt(clip.startTime)
      val end = fmt(clip.endTime)
      s"$prevBg[v$idx]overlay=x=0:y=0:enable='between(t,$start,$end)'$outBg"
    }

    val (videoFilters, finalVideoOut) = if (videoClipsWithTrack.isEmpty) then
      (Seq(s"color=c=black:s=${settings.width}x${settings.height}:r=${settings.fps}:d=${fmt(totalDuration)}[vout]"), "[vout]")
    else
      (Seq(baseVideoFilter) ++ videoFilterParts ++ overlayChains, "[vout]")

    val audioFilterParts = audioClipsWithTrack.zipWithIndex.map { case ((trackId, clip), idx) =>
      val fileIdx = fileIndexMap(extractFilePath(clip.sourceUrl))
      val delayMs = (clip.startTime * 1000).toLong
      val trimFilter = s"atrim=start=${fmt(clip.trimStart)}:duration=${fmt(clip.duration)},asetpts=PTS-STARTPTS"
      val delayFilter = if delayMs > 0 then s",adelay=$delayMs|$delayMs" else ""

      s"[$fileIdx:a]$trimFilter$delayFilter[a$idx]"
    }

    val (audioFilters, finalAudioOut) = if audioClipsWithTrack.isEmpty then
      (Seq(s"anullsrc=r=44100:cl=stereo:d=${fmt(totalDuration)}[aout]"), "[aout]")
    else if audioClipsWithTrack.size == 1 then
      (audioFilterParts, "[a0]")
    else
      val inputsMerged = (0 until audioClipsWithTrack.size).map(i => s"[a$i]").mkString
      val mixFilter = s"${inputsMerged}amix=inputs=${audioClipsWithTrack.size}:duration=longest:dropout_transition=0[aout]"
      (audioFilterParts :+ mixFilter, "[aout]")

    val allFilterChains = (videoFilters ++ audioFilters).mkString(";")

    val filterArgs = Seq("-filter_complex", allFilterChains, "-map", finalVideoOut, "-map", finalAudioOut)

    val outputArgs = Seq(
      "-y",
      "-preset", "veryfast",
      "-r", settings.fps.toString,
      "-t", fmt(totalDuration),
      settings.outputFile.getAbsolutePath
    )

    ExportCommand(
      executable = "ffmpeg",
      arguments = Seq("-loglevel", "error") ++ inputArgs ++ filterArgs ++ outputArgs
    )