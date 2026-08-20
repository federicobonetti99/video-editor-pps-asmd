package core.engine

import core.model.*
import java.net.URI
import java.io.File

object ExportCommandBuilder:

  private def extractFilePath(url: String): String =
    try
      val uri = URI.create(url)
      if uri.getScheme == "file" then new File(uri).getAbsolutePath
      else url.stripPrefix("file:")
    catch
      case _: Throwable => url.stripPrefix("file:").stripPrefix("/")

  def buildCommand(timeline: Timeline, settings: ExportSettings): ExportCommand =
    val videoInputs = timeline.videoTracks.flatMap(_.clips).map(c => extractFilePath(c.sourceUrl)).distinct
    val audioInputs = timeline.audioTracks.flatMap(_.clips).map(c => extractFilePath(c.sourceUrl)).distinct
    val allInputs = (videoInputs ++ audioInputs).distinct

    val inputArgs = allInputs.flatMap(input => Seq("-i", input))

    val outputArgs = Seq(
      "-y",
      "-r", settings.fps.toString,
      "-s", s"${settings.width}x${settings.height}",
      settings.outputFile.getAbsolutePath
    )

    ExportCommand(
      executable = "ffmpeg",
      arguments = inputArgs ++ outputArgs
    )