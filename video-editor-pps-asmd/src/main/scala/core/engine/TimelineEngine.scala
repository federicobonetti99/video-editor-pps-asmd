package core.engine

import core.model.*

sealed trait InsertionPolicy
case object InsertAndShift extends InsertionPolicy
case object Overwrite extends InsertionPolicy

object TimelineEngine:

  def addVideoClip(timeline: Timeline, trackId: Int, clip: VideoClip): Timeline =
    addVideoClip(timeline, trackId, clip, InsertAndShift)

  def addVideoClip(
                    timeline: Timeline,
                    trackId: Int,
                    clip: VideoClip,
                    policy: InsertionPolicy = InsertAndShift
                  ): Timeline =
    modifyVideoTrack(timeline, trackId) { track =>
      val validated = validateClipDuration(clip)
      val updatedClips = policy match
        case InsertAndShift => resolveInsertAndShift(track.clips, validated)
        case Overwrite      => resolveOverwrite(track.clips, validated)
      track.copy(clips = updatedClips)
    }

  def removeVideoClip(timeline: Timeline, trackId: Int, clipIndex: Int): Timeline =
    modifyVideoTrack(timeline, trackId)(track => track.copy(clips = removeClipGeneric(track.clips, clipIndex)))

  def cutVideoClip(timeline: Timeline, trackId: Int, clipIndex: Int, relativeCutTime: Double): Timeline =
    modifyVideoTrack(timeline, trackId)(track => track.copy(clips = cutClipGeneric(track.clips, clipIndex, relativeCutTime)))

  def snapVideoClips(timeline: Timeline, trackId: Int): Timeline =
    modifyVideoTrack(timeline, trackId)(track => track.copy(clips = snapClipsGeneric(track.clips)))

  def snapClipsTogether(timeline: Timeline, trackId: Int): Timeline =
    snapVideoClips(timeline, trackId)

  def getVideoClipsAtTime(timeline: Timeline, timestamp: Double): List[VideoClip] =
    timeline.videoTracks.flatMap { track =>
      track.clips.filter { clip =>
        timestamp >= clip.startTime && timestamp < (clip.startTime + clip.duration)
      }
    }


  def addAudioClip(timeline: Timeline, trackId: Int, clip: AudioClip): Timeline =
    modifyAudioTrack(timeline, trackId) { track =>
      val validated = validateClipDuration(clip)
      track.copy(clips = resolveInsertAndShift(track.clips, validated))
    }

  def removeAudioClip(timeline: Timeline, trackId: Int, clipIndex: Int): Timeline =
    modifyAudioTrack(timeline, trackId)(track => track.copy(clips = removeClipGeneric(track.clips, clipIndex)))

  def cutAudioClip(timeline: Timeline, trackId: Int, clipIndex: Int, relativeCutTime: Double): Timeline =
    modifyAudioTrack(timeline, trackId)(track => track.copy(clips = cutClipGeneric(track.clips, clipIndex, relativeCutTime)))

  def snapAudioClips(timeline: Timeline, trackId: Int): Timeline =
    modifyAudioTrack(timeline, trackId)(track => track.copy(clips = snapClipsGeneric(track.clips)))

  def importVideoWithAudio(
                            timeline: Timeline,
                            videoTrackId: Int,
                            audioTrackId: Int,
                            videoClip: VideoClip
                          ): Timeline =
    val timelineWithVideo = addVideoClip(timeline, videoTrackId, videoClip)
    val audioClip = AudioClip(
      sourceUrl = videoClip.sourceUrl,
      sourceLength = videoClip.sourceLength,
      startTime = videoClip.startTime,
      trimStart = videoClip.trimStart,
      duration = videoClip.duration,
      volumePoints = Nil
    )
    addAudioClip(timelineWithVideo, audioTrackId, audioClip)

  def cutVideoAndAudio(
                        timeline: Timeline,
                        videoTrackId: Int,
                        audioTrackId: Int,
                        clipIndex: Int,
                        relativeCutTime: Double
                      ): Timeline =
    val timelineWithVideoCut = cutVideoClip(timeline, videoTrackId, clipIndex, relativeCutTime)
    cutAudioClip(timelineWithVideoCut, audioTrackId, clipIndex, relativeCutTime)

  def snapAllTracks(timeline: Timeline, videoTrackId: Int, audioTrackId: Int): Timeline =
    val snappedVideo = snapVideoClips(timeline, videoTrackId)
    snapAudioClips(snappedVideo, audioTrackId)

  def updatePlaybackTime(
                          currentTime: Double,
                          state: PlayerState,
                          deltaTime: Double,
                          maxDuration: Double
                        ): Double = state match
    case Paused => currentTime
    case Playing(speed) =>
      val nextTime = currentTime + (deltaTime * speed)
      if nextTime >= maxDuration then maxDuration else nextTime

  private def validateClipDuration[C <: MediaClip](clip: C): C =
    val maxAvailableDuration = clip.sourceLength - clip.trimStart
    if clip.duration > maxAvailableDuration then
      clip.withTimes(clip.startTime, clip.trimStart, maxAvailableDuration).asInstanceOf[C]
    else
      clip

  private def removeClipGeneric[C <: MediaClip](clips: List[C], clipIndex: Int): List[C] =
    if clips.isDefinedAt(clipIndex) then
      clips.patch(clipIndex, Nil, 1)
    else
      clips

  private def cutClipGeneric[C <: MediaClip](clips: List[C], clipIndex: Int, relativeCutTime: Double): List[C] =
    if clips.isDefinedAt(clipIndex) then
      val originalClip = clips(clipIndex)

      if relativeCutTime <= 0.0 || relativeCutTime >= originalClip.duration then
        clips
      else
        val leftClip = originalClip.withTimes(
          newStartTime = originalClip.startTime,
          newTrimStart = originalClip.trimStart,
          newDuration = relativeCutTime
        ).asInstanceOf[C]

        val rightClip = originalClip.withTimes(
          newStartTime = originalClip.startTime + relativeCutTime,
          newTrimStart = originalClip.trimStart + relativeCutTime,
          newDuration = originalClip.duration - relativeCutTime
        ).asInstanceOf[C]

        clips.patch(clipIndex, List(leftClip, rightClip), 1)
    else
      clips

  private def snapClipsGeneric[C <: MediaClip](clips: List[C]): List[C] =
    clips.foldLeft(List.empty[C]) { (accumulated, currentClip) =>
      accumulated.lastOption match
        case Some(lastClip) =>
          val nextStartTime = lastClip.startTime + lastClip.duration
          accumulated :+ currentClip.withTimes(
            newStartTime = nextStartTime,
            newTrimStart = currentClip.trimStart,
            newDuration = currentClip.duration
          ).asInstanceOf[C]
        case None =>
          accumulated :+ currentClip.withTimes(
            newStartTime = 0.0,
            newTrimStart = currentClip.trimStart,
            newDuration = currentClip.duration
          ).asInstanceOf[C]
    }

  private def resolveInsertAndShift[C <: MediaClip](existingClips: List[C], newClip: C): List[C] =
    val insertTime = newClip.startTime
    val insertDuration = newClip.duration
    val insertEnd = insertTime + insertDuration

    val processedClips = existingClips.flatMap { clip =>
      val clipEnd = clip.startTime + clip.duration

      if clipEnd <= insertTime then
        List(clip)
      else if clip.startTime >= insertTime then
        List(clip.withTimes(
          newStartTime = clip.startTime + insertDuration,
          newTrimStart = clip.trimStart,
          newDuration = clip.duration
        ).asInstanceOf[C])
      else
        val firstPartDuration = insertTime - clip.startTime
        val secondPartDuration = clip.duration - firstPartDuration

        val firstPart = clip.withTimes(
          newStartTime = clip.startTime,
          newTrimStart = clip.trimStart,
          newDuration = firstPartDuration
        ).asInstanceOf[C]

        val secondPart = clip.withTimes(
          newStartTime = insertEnd,
          newTrimStart = clip.trimStart + firstPartDuration,
          newDuration = secondPartDuration
        ).asInstanceOf[C]

        List(firstPart, secondPart)
    }

    (newClip :: processedClips).sortBy(_.startTime)

  def moveVideoClip(timeline: Timeline, trackId: Int, clipIndex: Int, newStartTime: Double): Timeline =
    val clampedStartTime = Math.max(0.0, newStartTime)
    timeline.copy(
      videoTracks = timeline.videoTracks.map { track =>
        if track.id == trackId && track.clips.isDefinedAt(clipIndex) then
          val updatedClips = track.clips.updated(clipIndex, track.clips(clipIndex).copy(startTime = clampedStartTime))
          track.copy(clips = updatedClips)
        else track
      }
    )

  def moveAudioClip(timeline: Timeline, trackId: Int, clipIndex: Int, newStartTime: Double): Timeline =
    val clampedStartTime = Math.max(0.0, newStartTime)
    timeline.copy(
      audioTracks = timeline.audioTracks.map { track =>
        if track.id == trackId && track.clips.isDefinedAt(clipIndex) then
          val updatedClips = track.clips.updated(clipIndex, track.clips(clipIndex).copy(startTime = clampedStartTime))
          track.copy(clips = updatedClips)
        else track
      }
    )

  private def resolveOverwrite[C <: MediaClip](existingClips: List[C], newClip: C): List[C] =
    (newClip :: existingClips).sortBy(_.startTime)

  private def modifyVideoTrack(timeline: Timeline, trackId: Int)(f: VideoTrack => VideoTrack): Timeline =
    timeline.copy(videoTracks = timeline.videoTracks.map(t => if t.id == trackId then f(t) else t))

  private def modifyAudioTrack(timeline: Timeline, trackId: Int)(f: AudioTrack => AudioTrack): Timeline =
    timeline.copy(audioTracks = timeline.audioTracks.map(t => if t.id == trackId then f(t) else t))