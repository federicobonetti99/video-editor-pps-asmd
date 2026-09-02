package core.engine

import org.scalatest.funsuite.AnyFunSuite
import core.model.*
import java.io.File

class ExportCommandBuilderTest extends AnyFunSuite:

  private val defaultSettings = ExportSettings(
    outputFile = new File("out.mp4"),
    width = 1920,
    height = 1080,
    fps = 30
  )

  private def createSingleClipTimeline(effect: VideoEffect): Timeline =
    val clip = VideoClip(
      sourceUrl = "file:/video.mp4",
      sourceLength = 10.0,
      startTime = 0.0,
      trimStart = 0.0,
      duration = 5.0,
      effect = effect
    )
    Timeline(videoTracks = List(VideoTrack(id = 1, clips = List(clip))), audioTracks = Nil)

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

    val filterIndex = command.arguments.indexOf("-filter_complex")
    assert(filterIndex != -1)

    val filterString = command.arguments(filterIndex + 1)
    assert(filterString.contains("s=1920x1080"))
    assert(filterString.contains("r=60"))
  }

  test("filter_complex does not contain consecutive empty commas") {
    val effects = List(
      VideoEffect.None,
      VideoEffect.FadeIn(1.5),
      VideoEffect.Grayscale,
      VideoEffect.ZoomIn(1.5),
      VideoEffect.Shake(10.0, 2.0)
    )

    effects.foreach { effect =>
      val command = ExportCommandBuilder.buildCommand(createSingleClipTimeline(effect), defaultSettings)
      val filterIndex = command.arguments.indexOf("-filter_complex")
      assert(filterIndex != -1)

      val filterString = command.arguments(filterIndex + 1)
      assert(!filterString.contains(",,"), s"Filter chain contains empty filter (,,) for effect $effect: $filterString")
    }
  }

  test("crop expressions do not contain literal single quotes that trigger EINVAL -22") {
    val effects = List(
      VideoEffect.ZoomIn(2.0),
      VideoEffect.Shake(15.0, 3.0)
    )

    effects.foreach { effect =>
      val command = ExportCommandBuilder.buildCommand(createSingleClipTimeline(effect), defaultSettings)
      val filterIndex = command.arguments.indexOf("-filter_complex")
      assert(filterIndex != -1)

      val filterString = command.arguments(filterIndex + 1)
      assert(!filterString.contains("crop=w='"), s"crop w contains single quote: $filterString")
      assert(!filterString.contains(":h='"), s"crop h contains single quote: $filterString")
      assert(!filterString.contains(":x='"), s"crop x contains single quote: $filterString")
      assert(!filterString.contains(":y='"), s"crop y contains single quote: $filterString")
    }
  }

  test("overlay expression correctly protects between commas with single quotes") {
    val command = ExportCommandBuilder.buildCommand(createSingleClipTimeline(VideoEffect.None), defaultSettings)
    val filterIndex = command.arguments.indexOf("-filter_complex")
    val filterString = command.arguments(filterIndex + 1)

    assert(filterString.contains("enable='between(t,"), s"Overlay is missing single quotes around between(): $filterString")
  }

  test("floating point numbers use US dot separator to avoid locale EINVAL issues") {
    val timeline = createSingleClipTimeline(VideoEffect.FadeIn(1.25))
    val command = ExportCommandBuilder.buildCommand(timeline, defaultSettings)

    val filterIndex = command.arguments.indexOf("-filter_complex")
    assert(filterIndex != -1)

    val filterString = command.arguments(filterIndex + 1)
    assert(filterString.contains("d=1.250"))
    assert(!filterString.contains("1,250"))
  }