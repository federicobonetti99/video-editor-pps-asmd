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
    val safeDuration = Math.max(0.001, clipDuration)
    effect match
      case VideoEffect.None => ""
      case VideoEffect.Grayscale => ",hue=s=0"
      case VideoEffect.Sepia => ",colorchannelmixer=.393:.769:.189:0:.349:.686:.168:0:.272:.534:.131"
      case VideoEffect.Invert => ",negate"
      case VideoEffect.Brightness(level) =>
        val b = fmt(Math.max(-1.0, Math.min(1.0, level)))
        s",eq=brightness=$b"
      case VideoEffect.FadeIn(duration) =>
        val fadeDur = Math.min(duration, safeDuration)
        s",fade=t=in:st=0:d=${fmt(fadeDur)}"
      case VideoEffect.ZoomIn(targetScale) =>
        val zExpr = s"1.0+(${fmt(targetScale)}-1.0)*min(1.0,t/${fmt(safeDuration)})"
        s",scale=eval=frame:w='iw*($zExpr)':h='ih*($zExpr)',crop=w='iw/($zExpr)':h='ih/($zExpr)':x='(in_w-out_w)/2':y='(in_h-out_h)/2'"
      case VideoEffect.Shake(intensity, frequency) =>
        val maxOffset = fmt(intensity * 2, 1)
        val wx = fmt(2 * Math.PI * frequency, 4)
        val wy = fmt(2 * Math.PI * (frequency * 1.3), 4)
        val intVal = fmt(intensity, 2)
        s",crop=w='iw-$maxOffset':h='ih-$maxOffset':x='$intVal+$intVal*sin($wx*t)':y='$intVal+$intVal*cos($wy*t)',scale=w='iw+$maxOffset':h='ih+$maxOffset'"

  def buildCommand(timeline: Timeline, settings: ExportSettings): ExportCommand =
    val videoClipsWithTrack = timeline.videoTracks.sortBy(_.id).flatMap(t => t.clips.map(c => (t.id, c)))
    val audioClipsWithTrack = timeline.audioTracks.sortBy(_.id).flatMap(t => t.clips.map(c => (t.id, c)))

    val totalDuration =
      val maxV = videoClipsWithTrack.map(_._2.endTime).maxOption.getOrElse(0.0)
      val maxA = audioClipsWithTrack.map(_._2.endTime).maxOption.getOrElse(0.0)
      Math.max(1.0, Math.max(maxV, maxA))

    val allUniqueFiles = (videoClipsWithTrack.map(_._2.sourceUrl) ++ audioClipsWithTrack.map(_._2.sourceUrl))
      .distinct
      .map(extractFilePath)

    val fileIndexMap = allUniqueFiles.zipWithIndex.toMap
    val inputArgs = allUniqueFiles.flatMap(file => Seq("-i", file))

    val baseVideoFilter = s"color=c=black:s=${settings.width}x${settings.height}:r=${settings.fps}:d=${fmt(totalDuration)}[bg0]"

    val videoFilterParts = videoClipsWithTrack.zipWithIndex.map { case ((_, clip), idx) =>
      val fileIdx = fileIndexMap(extractFilePath(clip.sourceUrl))
      val effectFilter = formatEffect(clip.effect, clip.duration)
      val scaleFilter = s"scale=${settings.width}:${settings.height}:force_original_aspect_ratio=decrease,pad=${settings.width}:${settings.height}:(ow-iw)/2:(oh-ih)/2"
      val trimAndEffect = s"trim=start=${fmt(clip.trimStart)}:duration=${fmt(clip.duration)},setpts=PTS-STARTPTS$effectFilter,setpts=PTS-STARTPTS+${fmt(clip.startTime)}/TB"

      s"[$fileIdx:v]$trimAndEffect,$scaleFilter,setsar=1[v$idx]"
    }

    val overlayChains = videoClipsWithTrack.indices.map { idx =>
      val prevBg = if idx == 0 then "[bg0]" else s"[bg$idx]"
      val clip = videoClipsWithTrack(idx)._2
      val outBg = if idx == videoClipsWithTrack.size - 1 then "[vout]" else s"[bg${idx + 1}]"
      val start = fmt(clip.startTime)
      val end = fmt(clip.endTime)
      s"$prevBg[v$idx]overlay=x=0:y=0:enable='between(t,$start,$end)'$outBg"
    }

    val (videoFilters, finalVideoOut) =
      if videoClipsWithTrack.isEmpty then
        (Seq(s"color=c=black:s=${settings.width}x${settings.height}:r=${settings.fps}:d=${fmt(totalDuration)}[vout]"), "[vout]")
      else
        (Seq(baseVideoFilter) ++ videoFilterParts ++ overlayChains, "[vout]")

    val audioFilterParts = audioClipsWithTrack.zipWithIndex.map { case ((_, clip), idx) =>
      val fileIdx = fileIndexMap(extractFilePath(clip.sourceUrl))
      val delayMs = (clip.startTime * 1000).toLong
      val trimFilter = s"atrim=start=${fmt(clip.trimStart)}:duration=${fmt(clip.duration)},asetpts=PTS-STARTPTS"
      val delayFilter = if delayMs > 0 then s",adelay=$delayMs|$delayMs" else ""

      s"[$fileIdx:a]$trimFilter$delayFilter[a$idx]"
    }

    val (audioFilters, finalAudioOut) =
      if audioClipsWithTrack.isEmpty then
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