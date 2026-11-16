package app.ai.capabilities

import app.ai.domain.{AIDirectorError, StoryboardPlan}
import scala.util.boundary.Label

trait StoryboardPlannerCapability:
  def createStoryboard(story: String)(using Label[AIDirectorError]): StoryboardPlan