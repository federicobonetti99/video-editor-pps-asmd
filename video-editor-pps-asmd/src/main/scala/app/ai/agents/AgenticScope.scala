package app.ai.agents

import app.ai.domain.*
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

final class AgenticScope(val topic: String):
  @volatile var storyboard: Option[StoryboardPlan] = None
  private val assets = new ConcurrentHashMap[Int, SceneAsset]()
  private val traceLog = new ConcurrentHashMap[Int, String]()

  def putAsset(index: Int, asset: SceneAsset): Unit =
    assets.put(index, asset)
    traceLog.put(index, s"Scene #$index generated: ${asset.image.path}")

  def orderedAssets: List[SceneAsset] =
    assets.asScala.toList.sortBy(_._1).map(_._2)

  def executionTrace: List[String] =
    traceLog.asScala.toList.sortBy(_._1).map(_._2)