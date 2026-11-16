package app.ai.services

import app.ai.agents.{AgenticScope, MultiSceneDirectorWorkflow}
import app.ai.capabilities.{ImageGeneratorCapability, StoryboardPlannerCapability}
import app.ai.domain.*
import core.model.*

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
    val newClips = assets.map { asset =>
      val clip = ImageClip.create(
        sourceUrl = asset.image.path,
        startTime = cursor,
        duration = asset.directive.durationSeconds
      )
      cursor += asset.directive.durationSeconds
      clip
    }

    val updatedTracks = timeline.videoTracks.map { track =>
      if track.id == trackId then track.copy(clips = track.clips ++ newClips)
      else track
    }

    timeline.copy(videoTracks = updatedTracks)