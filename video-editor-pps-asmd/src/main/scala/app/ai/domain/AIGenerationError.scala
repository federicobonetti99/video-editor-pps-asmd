package app.ai.domain

import scala.util.control.NoStackTrace

case class AIGenerationError(reason: String) extends Exception(reason) with NoStackTrace
case class AIParseError(reason: String) extends Exception(reason) with NoStackTrace