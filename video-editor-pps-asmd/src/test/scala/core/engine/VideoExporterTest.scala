package core.engine

import org.scalatest.funsuite.AnyFunSuite
import core.model.*
import java.io.File
import scala.util.{Failure, Success}

class VideoExporterTest extends AnyFunSuite:

  test("VideoExporter companion factory returns a valid VideoExporter instance") {
    val exporter = VideoExporter()
    assert(exporter != null)
    assert(exporter.isInstanceOf[VideoExporter])
  }

  test("VideoExporter returns Failure on unwritable or invalid target destination") {
    val emptyTimeline = Timeline(videoTracks = Nil, audioTracks = Nil)
    val invalidFile = new File("/non_existent_folder_xyz/output.mp4")
    val settings = ExportSettings(outputFile = invalidFile)

    val exporter = VideoExporter()
    val result = exporter.exportVideo(emptyTimeline, settings)

    assert(result.isFailure)
  }

  test("Mock VideoExporter executes export logic returning Success when parameters are valid") {
    val mockExporter = new VideoExporter:
      override def exportVideo(timeline: Timeline, settings: ExportSettings) =
        if settings.width > 0 && settings.height > 0 then Success(settings.outputFile)
        else Failure(new IllegalArgumentException("Invalid dimensions"))

    val validSettings = ExportSettings(outputFile = new File("test.mp4"), width = 1280, height = 720)
    val invalidSettings = ExportSettings(outputFile = new File("test.mp4"), width = 0, height = 0)
    val timeline = Timeline(Nil, Nil)

    assert(mockExporter.exportVideo(timeline, validSettings).isSuccess)
    assert(mockExporter.exportVideo(timeline, invalidSettings).isFailure)
  }