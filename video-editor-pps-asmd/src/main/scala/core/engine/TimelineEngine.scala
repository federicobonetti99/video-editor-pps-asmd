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
    val maxAvailableDuration = clip.sourceLength - clip.trimStart
    val validatedClip = if clip.duration > maxAvailableDuration then
      clip.copy(duration = maxAvailableDuration)
    else
      clip
  
    val updatedTracks = timeline.videoTracks.map { track =>
      if track.id == trackId then
        val updatedClips = policy match
          case InsertAndShift => resolveInsertAndShift(track.clips, validatedClip)
          case Overwrite      => resolveOverwrite(track.clips, validatedClip)
        track.copy(clips = updatedClips)
      else
        track
    }
    timeline.copy(videoTracks = updatedTracks)
  private def resolveInsertAndShift(existingClips: List[VideoClip], newClip: VideoClip): List[VideoClip] =
    val insertTime = newClip.startTime
    val insertDuration = newClip.duration
    val insertEnd = insertTime + insertDuration
  
    val processedClips = existingClips.flatMap { clip =>
      val clipEnd = clip.startTime + clip.duration
  
      if clipEnd <= insertTime then
        List(clip)
      else if clip.startTime >= insertTime then
        List(clip.copy(startTime = clip.startTime + insertDuration))
      else
        val firstPartDuration = insertTime - clip.startTime
        val secondPartDuration = clip.duration - firstPartDuration
  
        val firstPart = clip.copy(
          duration = firstPartDuration
        )
        val secondPart = clip.copy(
          startTime = insertEnd,
          trimStart = clip.trimStart + firstPartDuration,
          duration = secondPartDuration
        )
        List(firstPart, secondPart)
    }
  
    (newClip :: processedClips).sortBy(_.startTime)
  
  private def resolveOverwrite(existingClips: List[VideoClip], newClip: VideoClip): List[VideoClip] =
    (newClip :: existingClips).sortBy(_.startTime)

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

  def snapClipsTogether(timeline: Timeline, trackId: Int): Timeline =
    val updatedTracks = timeline.videoTracks.map { track =>
      if track.id == trackId then
        val snappedClips = track.clips.foldLeft(List.empty[VideoClip]) { (accumulated, currentClip) =>
          accumulated.lastOption match
            case Some(lastClip) =>
              val nextStartTime = lastClip.startTime + lastClip.duration
              accumulated :+ currentClip.copy(startTime = nextStartTime)
            case None =>
              accumulated :+ currentClip.copy(startTime = 0.0)
        }
        track.copy(clips = snappedClips)
      else
        track
    }
    timeline.copy(videoTracks = updatedTracks)

  def getVideoClipsAtTime(timeline: Timeline, timestamp: Double): List[VideoClip] =
    timeline.videoTracks.flatMap { track =>
      track.clips.filter { clip =>
        timestamp >= clip.startTime && timestamp < (clip.startTime + clip.duration)
      }
    }

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