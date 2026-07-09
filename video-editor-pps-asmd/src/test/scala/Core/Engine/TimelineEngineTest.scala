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

  test("Cut a video clip into two distinct sub-clips at a relative time") {
    val clipToCut = VideoClip(
      sourceUrl = "video1.mp4",
      startTime = 10.0,
      trimStart = 0.0,
      duration = 10.0,
      sourceLength = 20.0,
      effect = VideoEffect.None
    )
    val timelineWithClip = TimelineEngine.addVideoClip(initialTimeline, trackId = 1, clip = clipToCut)

    val updatedTimeline = TimelineEngine.cutVideoClip(timelineWithClip, trackId = 1, clipIndex = 0, relativeCutTime = 4.0)
    val finalClips = updatedTimeline.videoTracks.head.clips

    finalClips should have size 2

    val leftClip = finalClips(0)
    leftClip.startTime shouldBe 10.0
    leftClip.duration shouldBe 4.0
    leftClip.trimStart shouldBe 0.0

    val rightClip = finalClips(1)
    rightClip.startTime shouldBe 14.0
    rightClip.duration shouldBe 6.0
    rightClip.trimStart shouldBe 4.0
  }

  test("Snap clips together to remove gaps and overlaps inside a video track") {
    val clip1 = VideoClip("v1.mp4", startTime = 0.0, trimStart = 0.0, duration = 5.0, sourceLength = 10.0, effect = VideoEffect.None)
    val clip2 = VideoClip("v2.mp4", startTime = 2.0, trimStart = 0.0, duration = 4.0, sourceLength = 10.0, effect = VideoEffect.None) // Overlaps clip1!
    val clip3 = VideoClip("v3.mp4", startTime = 20.0, trimStart = 0.0, duration = 3.0, sourceLength = 10.0, effect = VideoEffect.None) // Leaves a huge gap!

    val messyTrack = VideoTrack(id = 1, clips = List(clip1, clip2, clip3))
    val timelineWithMessyTrack = Timeline(videoTracks = List(messyTrack), audioTracks = List.empty)

    val updatedTimeline = TimelineEngine.snapClipsTogether(timelineWithMessyTrack, trackId = 1)
    val snappedClips = updatedTimeline.videoTracks.head.clips

    snappedClips(0).startTime shouldBe 0.0
    snappedClips(1).startTime shouldBe 5.0
    snappedClips(2).startTime shouldBe 9.0
  }