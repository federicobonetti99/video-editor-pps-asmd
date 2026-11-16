package app.ai.domain

import core.model.VideoEffect

final case class ScenePlan(
                            index: Int,
                            narration: String,
                            visualPrompt: String,
                            durationSeconds: Double,
                            effect: VideoEffect = VideoEffect.None
                          )

final case class StoryboardPlan(
                                 title: String,
                                 visualStyleBible: String,
                                 scenes: List[ScenePlan]
                               )

final case class SceneAsset(
                             plan: ScenePlan,
                             image: GeneratedImage,
                             audioPathOpt: Option[String] = None
                           )

final case class AIDirectorError(reason: String)