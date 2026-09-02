package core.engine

import core.model.*

sealed trait InsertionPolicy
case object InsertAndShift extends InsertionPolicy
case object Overwrite extends InsertionPolicy

object TimelineEngine:

  def addVideoTrack(timeline: Timeline): Timeline =
    val nextId = if timeline.videoTracks.isEmpty then 1 else timeline.videoTracks.map(_.id).max + 1
    timeline.copy(videoTracks = timeline.videoTracks :+ VideoTrack(id = nextId, clips = Nil))

  def addAudioTrack(timeline: Timeline): Timeline =
    val nextId = if timeline.audioTracks.isEmpty then 1 else timeline.audioTracks.map(_.id).max + 1
    timeline.copy(audioTracks = timeline.audioTracks :+ AudioTrack(id = nextId, clips = Nil))

  def removeVideoTrack(timeline: Timeline, trackId: Int): Timeline =
    timeline.copy(videoTracks = timeline.videoTracks.filterNot(_.id == trackId))

  def removeAudioTrack(timeline: Timeline, trackId: Int): Timeline =
    timeline.copy(audioTracks = timeline.audioTracks.filterNot(_.id == trackId))

  def addVideoClip(timeline: Timeline, trackId: Int, clip: VideoClip): Timeline =
    addVideoClip(timeline, trackId, clip, InsertAndShift)

  def addVideoClip(
                    timeline: Timeline,
                    trackId: Int,
                    clip: VideoClip,
                    policy: InsertionPolicy
                  ): Timeline =
    modifyVideoTrack(timeline, trackId): track =>
      val validated = validateClipDuration(clip)
      val updatedClips = policy match
        case InsertAndShift => resolveInsertAndShift(track.clips, validated)
        case Overwrite      => resolveOverwrite(track.clips, validated)
      track.copy(clips = updatedClips)

  def removeVideoClip(timeline: Timeline, trackId: Int, clipIndex: Int): Timeline =
    modifyVideoTrack(timeline, trackId)(track => track.copy(clips = removeClipGeneric(track.clips, clipIndex)))

  def cutVideoClip(timeline: Timeline, trackId: Int, clipIndex: Int, relativeCutTime: Double): Timeline =
    modifyVideoTrack(timeline, trackId)(track => track.copy(clips = cutClipGeneric(track.clips, clipIndex, relativeCutTime)))

  def snapVideoClips(timeline: Timeline, trackId: Int): Timeline =
    modifyVideoTrack(timeline, trackId)(track => track.copy(clips = snapClipsGeneric(track.clips)))

  def snapClipsTogether(timeline: Timeline, trackId: Int): Timeline =
    snapAllTracks(timeline)

  def applyEffectToVideoClip(
                              timeline: Timeline,
                              trackId: Int,
                              clipIndex: Int,
                              effect: VideoEffect
                            ): Timeline =
    modifyVideoTrack(timeline, trackId): track =>
      if track.clips.isDefinedAt(clipIndex) then
        val updatedClips = track.clips.updated(
          clipIndex,
          track.clips(clipIndex).copy(effect = effect)
        )
        track.copy(clips = updatedClips)
      else track

  def moveClip[C <: MediaClip](timeline: Timeline, target: C, newStartTime: Double): Timeline =
    val safeStartTime = Math.max(0.0, newStartTime)
    val targetEndTime = safeStartTime + target.duration

    def hasOverlap(other: MediaClip): Boolean =
      val otherEndTime = other.startTime + other.duration
      Math.max(safeStartTime, other.startTime) < Math.min(targetEndTime, otherEndTime)

    target match
      case v: VideoClip =>
        val updatedVideoTracks = timeline.videoTracks.map: track =>
          if track.clips.exists(_.isSameAs(v)) then
            val otherClips = track.clips.filterNot(_.isSameAs(v))
            if otherClips.exists(hasOverlap) then track
            else
              val updatedClips = track.clips.map: clip =>
                if clip.isSameAs(v) then clip.withTiming(clip.timing.copy(startTime = safeStartTime))
                else clip
              track.copy(clips = updatedClips.sortBy(_.startTime))
          else track
        timeline.copy(videoTracks = updatedVideoTracks)

      case a: AudioClip =>
        val updatedAudioTracks = timeline.audioTracks.map: track =>
          if track.clips.exists(_.isSameAs(a)) then
            val otherClips = track.clips.filterNot(_.isSameAs(a))
            if otherClips.exists(hasOverlap) then track
            else
              val updatedClips = track.clips.map: clip =>
                if clip.isSameAs(a) then clip.withTiming(clip.timing.copy(startTime = safeStartTime))
                else clip
              track.copy(clips = updatedClips.sortBy(_.startTime))
          else track
        timeline.copy(audioTracks = updatedAudioTracks)

  def moveClipToTrack[C <: MediaClip](
                                       timeline: Timeline,
                                       target: C,
                                       targetTrackId: Int,
                                       newStartTime: Double
                                     ): Timeline =
    val safeStartTime = Math.max(0.0, newStartTime)
    val targetEndTime = safeStartTime + target.duration

    def hasOverlap(other: MediaClip): Boolean =
      val otherEndTime = other.startTime + other.duration
      Math.max(safeStartTime, other.startTime) < Math.min(targetEndTime, otherEndTime)

    target match
      case v: VideoClip =>
        timeline.videoTracks.find(_.id == targetTrackId) match
          case Some(destTrack) =>
            val otherClipsInDest = destTrack.clips.filterNot(_.isSameAs(v))
            if otherClipsInDest.exists(hasOverlap) then timeline
            else
              val updatedSourceTracks = timeline.videoTracks.map: track =>
                if track.id == targetTrackId then
                  val updatedTarget = v.withTiming(v.timing.copy(startTime = safeStartTime))
                  track.copy(clips = (updatedTarget :: otherClipsInDest).sortBy(_.startTime))
                else
                  track.copy(clips = track.clips.filterNot(_.isSameAs(v)))
              timeline.copy(videoTracks = updatedSourceTracks)
          case None => timeline

      case a: AudioClip =>
        timeline.audioTracks.find(_.id == targetTrackId) match
          case Some(destTrack) =>
            val otherClipsInDest = destTrack.clips.filterNot(_.isSameAs(a))
            if otherClipsInDest.exists(hasOverlap) then timeline
            else
              val updatedSourceTracks = timeline.audioTracks.map: track =>
                if track.id == targetTrackId then
                  val updatedTarget = a.withTiming(a.timing.copy(startTime = safeStartTime))
                  track.copy(clips = (updatedTarget :: otherClipsInDest).sortBy(_.startTime))
                else
                  track.copy(clips = track.clips.filterNot(_.isSameAs(a)))
              timeline.copy(audioTracks = updatedSourceTracks)
          case None => timeline

  def getVideoClipsAtTime(timeline: Timeline, timestamp: Double): List[VideoClip] =
    timeline.videoTracks.flatMap: track =>
      track.clips.filter(_.containsTime(timestamp))

  def getAudioClipsAtTime(timeline: Timeline, timestamp: Double): List[AudioClip] =
    timeline.audioTracks.flatMap: track =>
      track.clips.filter(_.containsTime(timestamp))

  def addAudioClip(timeline: Timeline, trackId: Int, clip: AudioClip): Timeline =
    modifyAudioTrack(timeline, trackId): track =>
      val validated = validateClipDuration(clip)
      track.copy(clips = resolveInsertAndShift(track.clips, validated))

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
      timing = videoClip.timing,
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

  def snapAllTracks(timeline: Timeline): Timeline =
    timeline.copy(
      videoTracks = timeline.videoTracks.map(t => t.copy(clips = snapClipsGeneric(t.clips))),
      audioTracks = timeline.audioTracks.map(t => t.copy(clips = snapClipsGeneric(t.clips)))
    )

  def snapAllTracks(timeline: Timeline, videoTrackId: Int, audioTrackId: Int): Timeline =
    snapAllTracks(timeline)

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

  def moveVideoClip(timeline: Timeline, trackId: Int, clipIndex: Int, newStartTime: Double): Timeline =
    val clampedStartTime = Math.max(0.0, newStartTime)
    modifyVideoTrack(timeline, trackId): track =>
      if track.clips.isDefinedAt(clipIndex) then
        val target = track.clips(clipIndex)
        val updated = target.withTiming(target.timing.copy(startTime = clampedStartTime))
        track.copy(clips = track.clips.updated(clipIndex, updated))
      else track

  def moveAudioClip(timeline: Timeline, trackId: Int, clipIndex: Int, newStartTime: Double): Timeline =
    val clampedStartTime = Math.max(0.0, newStartTime)
    modifyAudioTrack(timeline, trackId): track =>
      if track.clips.isDefinedAt(clipIndex) then
        val target = track.clips(clipIndex)
        val updated = target.withTiming(target.timing.copy(startTime = clampedStartTime))
        track.copy(clips = track.clips.updated(clipIndex, updated))
      else track

  private def validateClipDuration[C <: MediaClip](clip: C): C =
    val maxAvailableDuration = clip.sourceLength - clip.trimStart
    if clip.duration > maxAvailableDuration then
      clip.withTiming(clip.timing.copy(duration = maxAvailableDuration))
    else
      clip

  private def removeClipGeneric[C <: MediaClip](clips: List[C], clipIndex: Int): List[C] =
    if clips.isDefinedAt(clipIndex) then clips.patch(clipIndex, Nil, 1)
    else clips

  private def cutClipGeneric[C <: MediaClip](clips: List[C], clipIndex: Int, relativeCutTime: Double): List[C] =
    if clips.isDefinedAt(clipIndex) then
      val originalClip = clips(clipIndex)
      if relativeCutTime <= 0.0 || relativeCutTime >= originalClip.duration then
        clips
      else
        val leftClip = originalClip.withTiming(
          originalClip.timing.copy(duration = relativeCutTime)
        )
        val rightClip = originalClip.withTiming(
          ClipTiming(
            startTime = originalClip.startTime + relativeCutTime,
            trimStart = originalClip.trimStart + relativeCutTime,
            duration = originalClip.duration - relativeCutTime
          )
        )
        clips.patch(clipIndex, List(leftClip, rightClip), 1)
    else
      clips

  private def snapClipsGeneric[C <: MediaClip](clips: List[C]): List[C] =
    clips.sortBy(_.startTime).foldLeft(List.empty[C]): (accumulated, currentClip) =>
      val nextStartTime = accumulated.lastOption.map(_.endTime).getOrElse(0.0)
      accumulated :+ currentClip.withTiming(currentClip.timing.copy(startTime = nextStartTime))

  private def resolveInsertAndShift[C <: MediaClip](existingClips: List[C], newClip: C): List[C] =
    val insertTime = newClip.startTime
    val insertDuration = newClip.duration
    val insertEnd = insertTime + insertDuration

    val processedClips = existingClips.flatMap: clip =>
      val clipEnd = clip.endTime
      if clipEnd <= insertTime then
        List(clip)
      else if clip.startTime >= insertTime then
        List(clip.withTiming(clip.timing.copy(startTime = clip.startTime + insertDuration)))
      else
        val firstPartDuration = insertTime - clip.startTime
        val secondPartDuration = clip.duration - firstPartDuration
        val firstPart = clip.withTiming(clip.timing.copy(duration = firstPartDuration))
        val secondPart = clip.withTiming(
          ClipTiming(
            startTime = insertEnd,
            trimStart = clip.trimStart + firstPartDuration,
            duration = secondPartDuration
          )
        )
        List(firstPart, secondPart)

    (newClip :: processedClips).sortBy(_.startTime)

  private def resolveOverwrite[C <: MediaClip](existingClips: List[C], newClip: C): List[C] =
    (newClip :: existingClips).sortBy(_.startTime)

  private def modifyVideoTrack(timeline: Timeline, trackId: Int)(f: VideoTrack => VideoTrack): Timeline =
    timeline.copy(videoTracks = timeline.videoTracks.map(t => if t.id == trackId then f(t) else t))

  private def modifyAudioTrack(timeline: Timeline, trackId: Int)(f: AudioTrack => AudioTrack): Timeline =
    timeline.copy(audioTracks = timeline.audioTracks.map(t => if t.id == trackId then f(t) else t))