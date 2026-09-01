package core.engine

import core.model.*
import java.io.{BufferedReader, File, InputStreamReader}
import scala.util.Try

trait VideoExporter:
  def exportVideo(timeline: Timeline, settings: ExportSettings): Try[File] =
    exportVideo(timeline, settings, _ => ())

  def exportVideo(
                   timeline: Timeline,
                   settings: ExportSettings,
                   onProgress: Double => Unit
                 ): Try[File]

object VideoExporter:
  def apply(): VideoExporter = new FFmpegVideoExporter()

  private val outTimeUsPattern = """out_time_us=(-?\d+)""".r
  private val outTimeMsPattern = """out_time_ms=(-?\d+)""".r
  private val outTimePattern = """(?:out_time|time)=(-?\d{2}):(\d{2}):(\d{2}(?:\.\d+)?)""".r

  def parseProgress(line: String, totalDuration: Double, onProgress: Double => Unit): Unit =
    if totalDuration > 0.0 then
      outTimeUsPattern.findFirstMatchIn(line) match
        case Some(m) =>
          val micros = m.group(1).toDouble
          val seconds = micros / 1000000.0
          val fraction = Math.min(1.0, Math.max(0.0, seconds / totalDuration))
          onProgress(fraction)
        case None =>
          outTimeMsPattern.findFirstMatchIn(line) match
            case Some(m) =>
              val millis = m.group(1).toDouble
              val seconds = millis / 1000.0
              val fraction = Math.min(1.0, Math.max(0.0, seconds / totalDuration))
              onProgress(fraction)
            case None =>
              outTimePattern.findFirstMatchIn(line).foreach: m =>
                val hours = m.group(1).toDouble
                val minutes = m.group(2).toDouble
                val seconds = m.group(3).toDouble
                val currentSeconds = (hours * 3600.0) + (minutes * 60.0) + seconds
                val fraction = Math.min(1.0, Math.max(0.0, currentSeconds / totalDuration))
                onProgress(fraction)
private class FFmpegVideoExporter extends VideoExporter:

  override def exportVideo(
                            timeline: Timeline,
                            settings: ExportSettings,
                            onProgress: Double => Unit
                          ): Try[File] =
    Try:
      val parentDir = settings.outputFile.getParentFile
      if parentDir != null && !parentDir.exists() then
        throw new IllegalArgumentException(s"Directory does not exist: ${parentDir.getAbsolutePath}")

      val totalDuration = {
        val maxV = timeline.videoTracks.flatMap(_.clips).map(_.endTime).maxOption.getOrElse(0.0)
        val maxA = timeline.audioTracks.flatMap(_.clips).map(_.endTime).maxOption.getOrElse(0.0)
        Math.max(1.0, Math.max(maxV, maxA))
      }

      val command = ExportCommandBuilder.buildCommand(timeline, settings)
      val fullCommand = Seq(command.executable) ++ command.arguments ++ Seq("-nostats", "-progress", "pipe:1")

      val processBuilder = new java.lang.ProcessBuilder(fullCommand*)
      processBuilder.redirectErrorStream(true)
      val process = processBuilder.start()

      val reader = new BufferedReader(new InputStreamReader(process.getInputStream))
      var line = reader.readLine()
      while line != null do
        VideoExporter.parseProgress(line, totalDuration, onProgress)
        line = reader.readLine()

      val exitCode = process.waitFor()
      reader.close()

      if exitCode == 0 then
        onProgress(1.0)
        settings.outputFile
      else
        throw new RuntimeException(s"FFmpeg export failed with exit code $exitCode")