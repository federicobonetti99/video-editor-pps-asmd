package core.model

sealed trait PlayerState
case object Paused extends PlayerState
case class Playing(speed: Double = 1.0) extends PlayerState