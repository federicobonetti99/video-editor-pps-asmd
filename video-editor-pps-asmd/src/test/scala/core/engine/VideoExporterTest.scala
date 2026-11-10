package core.engine

import org.scalatest.funsuite.AnyFunSuite
import core.model.*
import java.io.File
import scala.sys.process.*
import scala.util.Try

class VideoExporterTest extends AnyFunSuite:

  private def createSyntheticTestVideo(): File =
    val tempFile = File.createTempFile("test_input_", ".mp4")
    tempFile.deleteOnExit()
    val command = Seq(
      "ffmpeg", "-y",
      "-f", "lavfi",
      "-i", "color=c=blue:s=320x240:r=30",
      "-t", "1.0",
      "-c:v", "libx264",
      "-pix_fmt", "yuv420p",
      tempFile.getAbsolutePath
    )
    val exitCode = Process(command).!(ProcessLogger(_ => (), _ => ()))
    assert(exitCode == 0)
    tempFile

  private def parseSingleLine(line: String, totalDuration: Double): Option[Double] =
    val result = Vector.newBuilder[Double]
    VideoExporter.parseProgress(line, totalDuration, p => result += p)
    result.result().headOption

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
    val totalDuration = 20.0
    val p1 = parseSingleLine("out_time_us=5000000", totalDuration)
    val p2 = parseSingleLine("out_time_us=10000000", totalDuration)

    assert(p1.contains(0.25))
    assert(p2.contains(0.50))
  }

  test("VideoExporter.parseProgress correctly extracts progress from standard FFmpeg time string") {
    val totalDuration = 100.0
    val p = parseSingleLine("frame=  120 fps=60 time=00:00:50.00 bitrate=1500kbits/s", totalDuration)

    assert(p.contains(0.50))
  }

  test("VideoExporter.parseProgress clamps progress values between 0.0 and 1.0") {
    val totalDuration = 10.0
    val p1 = parseSingleLine("out_time_us=15000000", totalDuration)
    val p2 = parseSingleLine("out_time_us=-1000", totalDuration)

    assert(p1.contains(1.0))
    assert(p2.contains(0.0))
  }

  test("VideoExporter executes ZoomIn without throwing internal FFmpeg errors") {
    val inputVideoFile = createSyntheticTestVideo()
    val outputVideoFile = File.createTempFile("test_zoom_output_", ".mp4")
    outputVideoFile.deleteOnExit()

    val zoomInEffect = VideoEffect.ZoomIn(1.5)
    val clip = VideoClip(
      sourceUrl = s"file:${inputVideoFile.getAbsolutePath}",
      sourceLength = 1.0,
      startTime = 0.0,
      trimStart = 0.0,
      duration = 1.0,
      effect = zoomInEffect
    )
    val timeline = Timeline(videoTracks = List(VideoTrack(id = 1, clips = List(clip))), audioTracks = Nil)
    val settings = ExportSettings(outputFile = outputVideoFile, width = 1920, height = 1080, fps = 30)

    val exporter = VideoExporter()
    val result = exporter.exportVideo(timeline, settings)

    assert(result.isSuccess)
    assert(outputVideoFile.exists())
    assert(outputVideoFile.length() > 0)
  }