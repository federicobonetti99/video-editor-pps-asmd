package core.engine

import org.scalatest.funsuite.AnyFunSuite
import core.model.*
import java.io.File

class ExportCommandBuilderTest extends AnyFunSuite:

  test("ExportCommandBuilder generates an ffmpeg command containing all media inputs") {
    val clip1 = VideoClip(
      sourceUrl = "file:/video1.mp4",
      sourceLength = 10.0,
      startTime = 0.0,
      trimStart = 0.0,
      duration = 5.0,
      effect = VideoEffect.None
    )
    val clip2 = VideoClip(
      sourceUrl = "file:/video2.mp4",
      sourceLength = 10.0,
      startTime = 5.0,
      trimStart = 0.0,
      duration = 5.0,
      effect = VideoEffect.None
    )
    val track = VideoTrack(id = 1, clips = List(clip1, clip2))
    val timeline = Timeline(videoTracks = List(track), audioTracks = Nil)

    val settings = ExportSettings(outputFile = new File("out.mp4"))
    val command = ExportCommandBuilder.buildCommand(timeline, settings)

    assert(command.executable == "ffmpeg")
    assert(command.arguments.exists(_.contains("video1.mp4")))
    assert(command.arguments.exists(_.contains("video2.mp4")))
    assert(command.arguments.exists(_.endsWith("out.mp4")))
  }

  test("ExportCommandBuilder respects target resolution and fps settings") {
    val timeline = Timeline(videoTracks = Nil, audioTracks = Nil)
    val settings = ExportSettings(
      outputFile = new File("output.mp4"),
      width = 1920,
      height = 1080,
      fps = 60
    )
    val command = ExportCommandBuilder.buildCommand(timeline, settings)

    val fpsIndex = command.arguments.indexOf("-r")
    assert(fpsIndex != -1 && command.arguments(fpsIndex + 1) == "60")

    val sizeIndex = command.arguments.indexOf("-s")
    assert(sizeIndex != -1 && command.arguments(sizeIndex + 1) == "1920x1080")
  }