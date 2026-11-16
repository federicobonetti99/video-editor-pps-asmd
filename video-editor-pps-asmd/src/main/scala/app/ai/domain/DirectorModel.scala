package app.ai.domain

import core.model.VideoEffect

final case class SceneDirective(
                                 sceneNumber: Int,
                                 shotType: String,
                                 narration: String,
                                 visualPrompt: String,
                                 durationSeconds: Double = 5.0,
                                 effect: VideoEffect = VideoEffect.None
                               )

final case class StoryboardPlan(
                                 styleBible: String,
                                 scenes: List[SceneDirective]
                               )

final case class SceneAsset(
                             directive: SceneDirective,
                             image: GeneratedImage,
                             audioPathOpt: Option[String] = None
                           )

final case class AIDirectorError(reason: String)