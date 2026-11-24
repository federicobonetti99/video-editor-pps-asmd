package app.ai.services

import app.ai.agents.{AgenticScope, MultiSceneDirectorWorkflow}
import app.ai.capabilities.{AudioGeneratorCapability, ImageGeneratorCapability, StoryboardPlannerCapability}
import app.ai.domain.*
import core.engine.TimelineEngine
import core.model.*

import java.nio.file.Paths
import scala.concurrent.{ExecutionContext, Future}

final case class DirectedTimelineResult(
                                         timeline: Timeline,
                                         scope: AgenticScope
                                       )

object AIDirectorService:

  def generateAndAppendStory(
                              story: String,
                              timeline: Timeline,
                              trackId: Int = 1
                            )(using
                              planner: StoryboardPlannerCapability,
                              imageGenerator: ImageGeneratorCapability,
                              audioGenerator: AudioGeneratorCapability,
                              ec: ExecutionContext
                            ): Future[Either[AIDirectorError | AIGenerationError, DirectedTimelineResult]] =
    MultiSceneDirectorWorkflow.produceScenes(story).map {
      case Left(err) => Left(err)
      case Right(scope) =>
        val updatedTimeline = appendAssetsToTrack(timeline, trackId, scope.orderedAssets)
        Right(DirectedTimelineResult(updatedTimeline, scope))
    }

  def appendAssetsToTrack(
                           timeline: Timeline,
                           trackId: Int,
                           assets: List[SceneAsset]
                         ): Timeline =
    var cursor = timeline.currentTime
    val videoClipsBuf = scala.collection.mutable.ListBuffer.empty[VisualClip]
    val audioClipsBuf = scala.collection.mutable.ListBuffer.empty[AudioClip]

    assets.foreach { asset =>
      val audioDur = asset.audioDurationOpt.getOrElse(0.0)
      val sceneDuration = Math.max(asset.directive.durationSeconds, audioDur + 0.3)

      val imgUrl = toFileUri(asset.image.path)
      val vClip = ImageClip.create(
        sourceUrl = imgUrl,
        startTime = cursor,
        duration = sceneDuration,
        effect = asset.directive.effect
      )
      videoClipsBuf += vClip

      asset.audioPathOpt.foreach { rawAudioPath =>
        val audioUrl = toFileUri(rawAudioPath)
        val aClip = AudioClip(
          sourceUrl = audioUrl,
          sourceLength = audioDur,
          timing = ClipTiming(
            startTime = cursor,
            trimStart = 0.0,
            duration = audioDur
          ),
          volume = 1.0
        )
        audioClipsBuf += aClip
      }

      cursor += sceneDuration
    }

    val updatedVideoTracks = timeline.videoTracks.map { track =>
      if track.id == trackId then track.copy(clips = track.clips ++ videoClipsBuf.toList)
      else track
    }

    val timelineWithVideo = timeline.copy(videoTracks = updatedVideoTracks)

    if audioClipsBuf.isEmpty then
      timelineWithVideo
    else
      val (baseTimeline, targetAudioTrackId) = timelineWithVideo.audioTracks.headOption match
        case Some(t) => (timelineWithVideo, t.id)
        case None =>
          val withTrack = TimelineEngine.addAudioTrack(timelineWithVideo)
          (withTrack, withTrack.audioTracks.head.id)

      val updatedAudioTracks = baseTimeline.audioTracks.map { track =>
        if track.id == targetAudioTrackId then track.copy(clips = track.clips ++ audioClipsBuf.toList)
        else track
      }
      baseTimeline.copy(audioTracks = updatedAudioTracks)

  private def toFileUri(rawPath: String): String =
    if rawPath.startsWith("file:") then rawPath
    else Paths.get(rawPath).toAbsolutePath.toUri.toString