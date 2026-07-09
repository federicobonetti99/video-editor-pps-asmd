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