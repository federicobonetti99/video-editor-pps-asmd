package core.engine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import core.model.*

class TimelineEngineTest extends AnyFunSuite with Matchers:

  private val sampleClip = VideoClip(
    sourceUrl = "video1.mp4",
    sourceLength = 10.0,
    startTime = 0.0,
    trimStart = 0.0,
    duration = 10.0,
    VideoEffect.None
  )

  val secondSampleClip: VideoClip = VideoClip(
    sourceUrl = "video2.mp4",
    sourceLength = 5.0,
    startTime = 5.0,
    trimStart = 0.0,
    duration = 5.0,
    effect = VideoEffect.None
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

  test("Retrieve the active video clips at a specific timestamp") {
    val clip1 = VideoClip("v1.mp4", startTime = 0.0, trimStart = 0.0, duration = 5.0, sourceLength = 10.0, effect = VideoEffect.None)
    val clip2 = VideoClip("v2.mp4", startTime = 5.0, trimStart = 0.0, duration = 7.0, sourceLength = 10.0, effect = VideoEffect.None)

    val track = VideoTrack(id = 1, clips = List(clip1, clip2))
    val timeline = Timeline(videoTracks = List(track), audioTracks = List.empty)
    val activeClipsAtSeven = TimelineEngine.getVideoClipsAtTime(timeline, timestamp = 7.0)
    activeClipsAtSeven should have size 1
    activeClipsAtSeven.head shouldBe clip2

    val activeClipsAtEmptyTime = TimelineEngine.getVideoClipsAtTime(timeline, timestamp = 99.0)
    activeClipsAtEmptyTime shouldBe empty
  }

  test("addVideoClip using INSERT mode should split the existing clip and shift the subsequent ones") {
    val initialTrack = VideoTrack(id = 1, clips = Nil)
    val emptyTimeline = Timeline(videoTracks = List(initialTrack), audioTracks = Nil)
    val timelineWithFirstClip = TimelineEngine.addVideoClip(emptyTimeline, 1, sampleClip)
    val resultingTimeline = TimelineEngine.addVideoClip(timelineWithFirstClip, 1, secondSampleClip)
    val resultingClips = resultingTimeline.videoTracks.find(_.id == 1).get.clips
    assert(resultingClips.size == 3, s"Expected 3 clips, but found ${resultingClips.size}")

    val sortedClips = resultingClips.sortBy(_.startTime)

    val firstPart = sortedClips(0)
    val insertedClip = sortedClips(1)
    val secondPart = sortedClips(2)

    assert(firstPart.sourceUrl == "video1.mp4")
    assert(firstPart.startTime == 0.0)
    assert(firstPart.duration == 5.0, s"Expected first part duration to be 5.0, got ${firstPart.duration}")

    assert(insertedClip.sourceUrl == "video2.mp4")
    assert(insertedClip.startTime == 5.0)
    assert(insertedClip.duration == 5.0)

    assert(secondPart.sourceUrl == "video1.mp4")
    assert(secondPart.startTime == 10.0, s"Expected second part to shift to 10.0, but was at ${secondPart.startTime}")
    assert(secondPart.duration == 5.0)
  }

  test("Automatically create and add an audio clip when a video clip is imported (Strada A)") {
    val videoTrack = VideoTrack(id = 1, clips = Nil)
    val audioTrack = AudioTrack(id = 1, clips = Nil)
    val emptyTimeline = Timeline(videoTracks = List(videoTrack), audioTracks = List(audioTrack))

    val importedVideo = VideoClip(
      sourceUrl = "file_con_audio.mp4",
      sourceLength = 12.0,
      startTime = 0.0,
      trimStart = 0.0,
      duration = 12.0,
      effect = VideoEffect.None
    )

    val updatedTimeline = TimelineEngine.importVideoWithAudio(emptyTimeline, videoTrackId = 1, audioTrackId = 1, videoClip = importedVideo)

    val finalVideoClips = updatedTimeline.videoTracks.find(_.id == 1).get.clips
    finalVideoClips should have size 1
    finalVideoClips.head shouldBe importedVideo

    val finalAudioClips = updatedTimeline.audioTracks.find(_.id == 1).get.clips
    finalAudioClips should have size 1

    val generatedAudio = finalAudioClips.head
    generatedAudio.sourceUrl shouldBe "file_con_audio.mp4"
    generatedAudio.startTime shouldBe 0.0
    generatedAudio.duration shouldBe 12.0
    generatedAudio.trimStart shouldBe 0.0
  }

  test("Ensure that cutting a video clip also cuts the synchronized audio clip") {
    val videoTrack = VideoTrack(id = 1, clips = List(sampleClip))
    val audioClip = AudioClip(
      sourceUrl = "video1.mp4",
      sourceLength = 10.0,
      startTime = 0.0,
      trimStart = 0.0,
      duration = 10.0,
      volumePoints = List.empty
    )

    val audioTrack = AudioTrack(id = 1, clips = List(audioClip))
    val timeline = Timeline(videoTracks = List(videoTrack), audioTracks = List(audioTrack))

    val updatedTimeline = TimelineEngine.cutVideoAndAudio(timeline, videoTrackId = 1, audioTrackId = 1, clipIndex = 0, relativeCutTime = 4.0)

    val finalVideoClips = updatedTimeline.videoTracks.find(_.id == 1).get.clips
    finalVideoClips should have size 2
    finalVideoClips(0).duration shouldBe 4.0
    finalVideoClips(1).duration shouldBe 6.0

    val finalAudioClips = updatedTimeline.audioTracks.find(_.id == 1).get.clips
    finalAudioClips should have size 2
    finalAudioClips(0).duration shouldBe 4.0
    finalAudioClips(1).duration shouldBe 6.0
    finalAudioClips(1).trimStart shouldBe 4.0
  }

  test("Snap clips together on both video and audio tracks simultaneously") {
    val videoClip1 = VideoClip("v1.mp4", startTime = 0.0, trimStart = 0.0, duration = 4.0, sourceLength = 10.0, effect = VideoEffect.None)
    val videoClip2 = VideoClip("v2.mp4", startTime = 10.0, trimStart = 0.0, duration = 5.0, sourceLength = 10.0, effect = VideoEffect.None)
    val videoTrack = VideoTrack(id = 1, clips = List(videoClip1, videoClip2))

    val audioClip1 = AudioClip("a1.mp3", startTime = 0.0, trimStart = 0.0, duration = 3.0, sourceLength = 10.0, volumePoints = Nil)
    val audioClip2 = AudioClip("a2.mp3", startTime = 12.0, trimStart = 0.0, duration = 6.0, sourceLength = 10.0, volumePoints = Nil)
    val audioTrack = AudioTrack(id = 1, clips = List(audioClip1, audioClip2))

    val timeline = Timeline(videoTracks = List(videoTrack), audioTracks = List(audioTrack))
    val updatedTimeline = TimelineEngine.snapAllTracks(timeline, videoTrackId = 1, audioTrackId = 1)

    val snappedVideoClips = updatedTimeline.videoTracks.head.clips
    snappedVideoClips(0).startTime shouldBe 0.0
    snappedVideoClips(1).startTime shouldBe 4.0

    val snappedAudioClips = updatedTimeline.audioTracks.head.clips
    snappedAudioClips(0).startTime shouldBe 0.0
    snappedAudioClips(1).startTime shouldBe 3.0
  }

  test("Snap must sort clips by time before snapping to prevent gaps when clips are out of list order") {
    val disorderedAudio1 = AudioClip("a1.mp3", startTime = 15.0, trimStart = 0.0, duration = 5.0, sourceLength = 20.0, volumePoints = Nil)
    val disorderedAudio2 = AudioClip("a2.mp3", startTime = 2.0, trimStart = 0.0, duration = 4.0, sourceLength = 20.0, volumePoints = Nil)

    val track = AudioTrack(id = 1, clips = List(disorderedAudio1, disorderedAudio2))
    val timeline = Timeline(videoTracks = Nil, audioTracks = List(track))

    val updatedTimeline = TimelineEngine.snapAudioClips(timeline, trackId = 1)
    val snappedClips = updatedTimeline.audioTracks.head.clips

    snappedClips(0).startTime shouldBe 0.0
    snappedClips(0).sourceUrl shouldBe "a2.mp3"
    snappedClips(1).startTime shouldBe 4.0
    snappedClips(1).sourceUrl shouldBe "a1.mp3"
  }

  test("moveClip should update startTime and sort clips chronologically for video") {
    val clip1 = VideoClip("v1.mp4", startTime = 0.0, trimStart = 0.0, duration = 5.0, sourceLength = 10.0, effect = VideoEffect.None)
    val clip2 = VideoClip("v2.mp4", startTime = 5.0, trimStart = 0.0, duration = 5.0, sourceLength = 10.0, effect = VideoEffect.None)
    val timeline = Timeline(videoTracks = List(VideoTrack(1, List(clip1, clip2))), audioTracks = Nil)

    val updated = TimelineEngine.moveClip(timeline, target = clip1, newStartTime = 12.0)
    val clips = updated.videoTracks.head.clips

    clips(0).sourceUrl shouldBe "v2.mp4"
    clips(0).startTime shouldBe 5.0
    clips(1).sourceUrl shouldBe "v1.mp4"
    clips(1).startTime shouldBe 12.0
  }

  test("moveClip backwards should correctly place the clip before earlier clips") {
    val clip1 = VideoClip("v1.mp4", startTime = 10.0, trimStart = 0.0, duration = 4.0, sourceLength = 10.0, effect = VideoEffect.None)
    val clip2 = VideoClip("v2.mp4", startTime = 20.0, trimStart = 0.0, duration = 4.0, sourceLength = 10.0, effect = VideoEffect.None)
    val timeline = Timeline(videoTracks = List(VideoTrack(1, List(clip1, clip2))), audioTracks = Nil)

    val updated = TimelineEngine.moveClip(timeline, target = clip2, newStartTime = 2.0)
    val clips = updated.videoTracks.head.clips

    clips(0).sourceUrl shouldBe "v2.mp4"
    clips(0).startTime shouldBe 2.0
    clips(1).sourceUrl shouldBe "v1.mp4"
    clips(1).startTime shouldBe 10.0
  }

  test("moveClip should clamp newStartTime to 0.0 if negative") {
    val clip = VideoClip("v1.mp4", startTime = 5.0, trimStart = 0.0, duration = 3.0, sourceLength = 10.0, effect = VideoEffect.None)
    val timeline = Timeline(videoTracks = List(VideoTrack(1, List(clip))), audioTracks = Nil)

    val updated = TimelineEngine.moveClip(timeline, target = clip, newStartTime = -4.0)

    updated.videoTracks.head.clips.head.startTime shouldBe 0.0
  }

  test("moveClip should work polymorphically on audio clips") {
    val audio1 = AudioClip("a1.mp3", startTime = 0.0, trimStart = 0.0, duration = 3.0, sourceLength = 10.0, volumePoints = Nil)
    val audio2 = AudioClip("a2.mp3", startTime = 4.0, trimStart = 0.0, duration = 3.0, sourceLength = 10.0, volumePoints = Nil)
    val timeline = Timeline(videoTracks = Nil, audioTracks = List(AudioTrack(1, List(audio1, audio2))))

    val updated = TimelineEngine.moveClip(timeline, target = audio1, newStartTime = 8.0)
    val clips = updated.audioTracks.head.clips

    clips(0).sourceUrl shouldBe "a2.mp3"
    clips(0).startTime shouldBe 4.0
    clips(1).sourceUrl shouldBe "a1.mp3"
    clips(1).startTime shouldBe 8.0
  }

  test("moveClip should NOT move the clip if the new position overlaps with another clip on the same track") {
    val clip1 = VideoClip("v1.mp4", startTime = 0.0, trimStart = 0.0, duration = 5.0, sourceLength = 10.0, effect = VideoEffect.None)
    val clip2 = VideoClip("v2.mp4", startTime = 10.0, trimStart = 0.0, duration = 5.0, sourceLength = 10.0, effect = VideoEffect.None)
    val timeline = Timeline(videoTracks = List(VideoTrack(1, List(clip1, clip2))), audioTracks = Nil)

    val updated = TimelineEngine.moveClip(timeline, target = clip2, newStartTime = 3.0)

    val clips = updated.videoTracks.head.clips
    clips(0).startTime shouldBe 0.0
    clips(1).startTime shouldBe 10.0
  }

  test("Dynamically add video and audio tracks with auto-incrementing IDs") {
    val t0 = Timeline(videoTracks = Nil, audioTracks = Nil)
    val t1 = TimelineEngine.addVideoTrack(t0)
    val t2 = TimelineEngine.addVideoTrack(t1)
    val t3 = TimelineEngine.addAudioTrack(t2)

    t3.videoTracks.map(_.id) shouldBe List(1, 2)
    t3.audioTracks.map(_.id) shouldBe List(1)
  }

  test("Remove a specific video track and keep the others untouched") {
    val t0 = Timeline(
      videoTracks = List(VideoTrack(1, Nil), VideoTrack(2, List(sampleClip)), VideoTrack(3, Nil)),
      audioTracks = Nil
    )
    val updated = TimelineEngine.removeVideoTrack(t0, trackId = 2)

    updated.videoTracks.map(_.id) shouldBe List(1, 3)
    updated.videoTracks.flatMap(_.clips) shouldBe empty
  }

  test("Retrieve all audio clips active at a specific timestamp across multiple audio tracks") {
    val clip1 = AudioClip("a1.mp3", startTime = 0.0, trimStart = 0.0, duration = 10.0, sourceLength = 20.0, volumePoints = Nil)
    val clip2 = AudioClip("a2.mp3", startTime = 5.0, trimStart = 0.0, duration = 10.0, sourceLength = 20.0, volumePoints = Nil)

    val timeline = Timeline(
      videoTracks = Nil,
      audioTracks = List(
        AudioTrack(1, List(clip1)),
        AudioTrack(2, List(clip2))
      )
    )

    // At t = 2.0 only clip1 is playing
    TimelineEngine.getAudioClipsAtTime(timeline, timestamp = 2.0) shouldBe List(clip1)

    // At t = 7.0 both clips overlap on different tracks
    val activeAtSeven = TimelineEngine.getAudioClipsAtTime(timeline, timestamp = 7.0)
    activeAtSeven should have size 2
    activeAtSeven should contain allOf(clip1, clip2)
  }

  test("moveClipToTrack should transfer a video clip to another track if there are no overlaps") {
    val track1 = VideoTrack(1, List(sampleClip))
    val track2 = VideoTrack(2, Nil)
    val timeline = Timeline(videoTracks = List(track1, track2), audioTracks = Nil)

    val updated = TimelineEngine.moveClipToTrack(timeline, target = sampleClip, targetTrackId = 2, newStartTime = 15.0)

    updated.videoTracks.find(_.id == 1).get.clips shouldBe empty
    val destClips = updated.videoTracks.find(_.id == 2).get.clips
    destClips should have size 1
    destClips.head.sourceUrl shouldBe sampleClip.sourceUrl
    destClips.head.startTime shouldBe 15.0
  }

  test("moveClipToTrack should reject move if destination track has an overlapping clip") {
    val obstacleClip = VideoClip("obs.mp4", startTime = 10.0, trimStart = 0.0, duration = 5.0, sourceLength = 10.0, effect = VideoEffect.None)
    val track1 = VideoTrack(1, List(sampleClip)) // 0.0 to 10.0
    val track2 = VideoTrack(2, List(obstacleClip)) // 10.0 to 15.0
    val timeline = Timeline(videoTracks = List(track1, track2), audioTracks = Nil)

    // Try moving sampleClip to track 2 at startTime 12.0 (overlaps obstacleClip 10.0-15.0)
    val updated = TimelineEngine.moveClipToTrack(timeline, target = sampleClip, targetTrackId = 2, newStartTime = 12.0)

    // Track 1 should still have sampleClip and track 2 should still have obstacleClip untouched
    updated.videoTracks.find(_.id == 1).get.clips shouldBe List(sampleClip)
    updated.videoTracks.find(_.id == 2).get.clips shouldBe List(obstacleClip)
  }