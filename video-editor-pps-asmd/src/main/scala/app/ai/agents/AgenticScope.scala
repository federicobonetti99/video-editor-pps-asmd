package app.ai.agents

import app.ai.domain.*
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

final class AgenticScope(val story: String):
  @volatile private var currentPlan: Option[StoryboardPlan] = None
  private val generatedAssets = new ConcurrentHashMap[Int, SceneAsset]()
  private val traces = new ConcurrentHashMap[Int, String]()

  def setPlan(plan: StoryboardPlan): Unit =
    currentPlan = Some(plan)

  def plan: Option[StoryboardPlan] = currentPlan

  def recordAsset(asset: SceneAsset): Unit =
    val idx = asset.directive.sceneNumber
    generatedAssets.put(idx, asset)
    traces.put(idx, s"Scene #$idx [${asset.directive.shotType}] -> ${asset.image.path}")

  def orderedAssets: List[SceneAsset] =
    generatedAssets.asScala.toList.sortBy(_._1).map(_._2)

  def executionTrace: List[String] =
    traces.asScala.toList.sortBy(_._1).map(_._2)