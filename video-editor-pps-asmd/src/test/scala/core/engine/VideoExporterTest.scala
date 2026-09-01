package core.engine

import org.scalatest.funsuite.AnyFunSuite
import core.model.*
import java.io.File
import scala.util.Try

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

  test("VideoExporter.parseProgress correctly extracts progress from FFmpeg out_time_us") {
    var progress = 0.0
    val totalDuration = 20.0

    VideoExporter.parseProgress("out_time_us=5000000", totalDuration, p => progress = p)
    assert(progress == 0.25)

    VideoExporter.parseProgress("out_time_us=10000000", totalDuration, p => progress = p)
    assert(progress == 0.50)
  }

  test("VideoExporter.parseProgress correctly extracts progress from standard FFmpeg time string") {
    var progress = 0.0
    val totalDuration = 100.0

    VideoExporter.parseProgress("frame=  120 fps=60 time=00:00:50.00 bitrate=1500kbits/s", totalDuration, p => progress = p)
    assert(progress == 0.50)
  }

  test("VideoExporter.parseProgress clamps progress values between 0.0 and 1.0") {
    var progress = 0.0
    val totalDuration = 10.0

    VideoExporter.parseProgress("out_time_us=15000000", totalDuration, p => progress = p)
    assert(progress == 1.0)

    VideoExporter.parseProgress("out_time_us=-1000", totalDuration, p => progress = p)
    assert(progress == 0.0)
  }