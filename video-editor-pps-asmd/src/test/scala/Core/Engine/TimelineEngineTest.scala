package Core.Engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import Core.Model.*

class TimelineEngineTest extends AnyFunSuite with Matchers:

  private val sampleClip = VideoClip(
    sourceUrl = "video1.mp4",
    sourceLength = 10.0,
    startTime = 0.0,
    trimStart = 0.0,
    duration = 10.0,
    VideoEffect.None
  )
  private val emptyTrack = VideoTrack(id = 1, clips = List.empty)
  private val initialTimeline = Timeline(videoTracks = List(emptyTrack), audioTracks = List.empty)

  test("Correctly add a video clip to an existing track") {
    val updatedTimeline = TimelineEngine.addVideoClip(initialTimeline, trackId = 1, clip = sampleClip)

    updatedTimeline.videoTracks.head.clips should have size 1
    updatedTimeline.videoTracks.head.clips.head shouldBe sampleClip
  }

  test("Ensure immutability by keeping the starting timeline unchanged") {
    val _ = TimelineEngine.addVideoClip(initialTimeline, trackId = 1, clip = sampleClip)

    initialTimeline.videoTracks.head.clips shouldBe empty
  }

  test("Remove a video clip given its positional list index") {
    val timelineWithClip = TimelineEngine.addVideoClip(initialTimeline, trackId = 1, clip = sampleClip)
    val timelineWithoutClip = TimelineEngine.removeVideoClip(timelineWithClip, trackId = 1, clipIndex = 0)

    timelineWithoutClip.videoTracks.head.clips shouldBe empty
  }

  test("Return the exact same timeline if the clip index to remove does not exist") {
    val updatedTimeline = TimelineEngine.removeVideoClip(initialTimeline, trackId = 1, clipIndex = 999)

    updatedTimeline shouldBe initialTimeline
  }

  test("Cap video clip duration if it exceeds the original video file length") {
    val videoLength = 10.0
    val oversizedClip = VideoClip(
      sourceUrl = "video1.mp4",
      sourceLength = 5.0,
      startTime = 0.0,
      trimStart = 0.0,
      duration = videoLength,
      VideoEffect.None
    )

    val updatedTimeline = TimelineEngine.addVideoClip(initialTimeline, trackId = 1, clip = oversizedClip)
    val addedClip = updatedTimeline.videoTracks.head.clips.head

    addedClip.duration shouldBe 5.0
  }