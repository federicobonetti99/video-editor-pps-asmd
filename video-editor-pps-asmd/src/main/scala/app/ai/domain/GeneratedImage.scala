package app.ai.domain

case class GeneratedImage(
                           path: String,
                           width: Int,
                           height: Int,
                           prompt: String
                         ):
  def aspectRatio: Double =
    if height == 0 then 0.0 else width.toDouble / height