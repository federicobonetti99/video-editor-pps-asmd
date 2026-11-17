package app.ai.domain

final case class GeneratedAudio(
                                 path: String,
                                 durationSeconds: Double,
                                 prompt: String
                               )