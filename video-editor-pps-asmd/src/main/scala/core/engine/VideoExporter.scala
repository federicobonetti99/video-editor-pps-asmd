package core.engine

import core.model.*
import java.io.File
import scala.sys.process.*
import scala.util.Try

trait VideoExporter:
  def exportVideo(timeline: Timeline, settings: ExportSettings): Try[File]

object VideoExporter:
  def apply(): VideoExporter = new FFmpegVideoExporter()

private class FFmpegVideoExporter extends VideoExporter:
  override def exportVideo(timeline: Timeline, settings: ExportSettings): Try[File] =
    Try:
      val parentDir = settings.outputFile.getParentFile
      if parentDir != null && !parentDir.exists() then
        throw new IllegalArgumentException(s"Directory does not exist: ${parentDir.getAbsolutePath}")

      val command = ExportCommandBuilder.buildCommand(timeline, settings)
      val fullCommand = Seq(command.executable) ++ command.arguments
      val exitCode = Process(fullCommand).!
      if exitCode == 0 then settings.outputFile
      else throw new RuntimeException(s"FFmpeg export failed with exit code $exitCode")