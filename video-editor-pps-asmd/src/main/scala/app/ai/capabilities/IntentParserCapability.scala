package app.ai.capabilities

import app.ai.domain.{AIParseError, UserIntent}
import scala.util.boundary.Label

trait IntentParserCapability:
  def parse(text: String)(using Label[AIParseError]): UserIntent