package Core.Engine

import Core.Model.*

object TimelineEngine:

  def addVideoClip(timeline: Timeline, trackId: Int, clip: VideoClip): Timeline =
    val maxAvailableDuration = clip.sourceLength - clip.trimStart
    val validatedDuration = Math.min(clip.duration, maxAvailableDuration)
    val validatedClip = clip.copy(duration = validatedDuration)

    val updatedTracks = timeline.videoTracks.map { track =>
      if track.id == trackId then
        track.copy(clips = track.clips :+ validatedClip)
      else
        track
    }
    timeline.copy(videoTracks = updatedTracks)

  def removeVideoClip(timeline: Timeline, trackId: Int, clipIndex: Int): Timeline =
    val updatedTracks = timeline.videoTracks.map { track =>
      if track.id == trackId && track.clips.isDefinedAt(clipIndex) then
        track.copy(clips = track.clips.patch(clipIndex, Nil, 1))
      else
        track
    }
    timeline.copy(videoTracks = updatedTracks)

  def cutVideoClip(timeline: Timeline, trackId: Int, clipIndex: Int, relativeCutTime: Double): Timeline =
    val updatedTracks = timeline.videoTracks.map { track =>
      if track.id == trackId && track.clips.isDefinedAt(clipIndex) then
        val originalClip = track.clips(clipIndex)

        if relativeCutTime <= 0.0 || relativeCutTime >= originalClip.duration then
          track
        else
          val leftClip = originalClip.copy(
            duration = relativeCutTime
          )
          val rightClip = originalClip.copy(
            startTime = originalClip.startTime + relativeCutTime,
            trimStart = originalClip.trimStart + relativeCutTime,
            duration = originalClip.duration - relativeCutTime
          )

          track.copy(clips = track.clips.patch(clipIndex, List(leftClip, rightClip), 1))
      else
        track
    }
    timeline.copy(videoTracks = updatedTracks)