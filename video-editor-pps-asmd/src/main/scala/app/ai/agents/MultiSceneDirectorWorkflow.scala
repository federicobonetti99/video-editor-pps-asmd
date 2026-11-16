package app.ai.agents

import app.ai.capabilities.{ImageGeneratorCapability, StoryboardPlannerCapability}
import app.ai.domain.*

import scala.concurrent.{ExecutionContext, Future}
import scala.util.boundary

object MultiSceneDirectorWorkflow:

  def produceScenes(story: String)(using
                                   planner: StoryboardPlannerCapability,
                                   imageGenerator: ImageGeneratorCapability,
                                   ec: ExecutionContext
  ): Future[Either[AIDirectorError | AIGenerationError, AgenticScope]] =
    val scope = new AgenticScope(story)

    val planOutcome = boundary[AIDirectorError | StoryboardPlan]:
      planner.createStoryboard(story)

    planOutcome match
      case err: AIDirectorError =>
        Future.successful(Left(err))

      case storyboard: StoryboardPlan =>
        scope.setPlan(storyboard)

        val sceneFutures: List[Future[Either[AIGenerationError, SceneAsset]]] =
          storyboard.scenes.map { directive =>
            Future {
              val enrichedPrompt =
                if storyboard.styleBible.isBlank || directive.visualPrompt.contains(storyboard.styleBible) then
                  directive.visualPrompt
                else
                  s"${storyboard.styleBible}. ${directive.visualPrompt}"

              val imgOutcome = boundary[AIGenerationError | GeneratedImage]:
                imageGenerator.generateImage(enrichedPrompt)

              imgOutcome match
                case err: AIGenerationError => Left(err)
                case img: GeneratedImage    => Right(SceneAsset(directive, img))
            }
          }

        Future.sequence(sceneFutures).map { results =>
          results.collectFirst { case Left(err) => err } match
            case Some(firstError) => Left(firstError)
            case None =>
              results.collect { case Right(asset) => asset }.foreach(scope.recordAsset)
              Right(scope)
        }