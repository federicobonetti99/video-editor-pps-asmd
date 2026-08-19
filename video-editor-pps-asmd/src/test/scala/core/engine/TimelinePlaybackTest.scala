package core.engine

import org.scalatest.funsuite.AnyFunSuite
import core.model.*

class TimelinePlaybackTest extends AnyFunSuite:

  val maxTimelineDuration = 60.0

  test("When the player is PAUSED, playback time should not advance even if real time passes") {
    val currentTime = 10.0
    val deltaTime = 0.5

    val resultTime = TimelineEngine.updatePlaybackTime(
      currentTime,
      Paused,
      deltaTime,
      maxTimelineDuration
    )

    assert(resultTime == 10.0, s"Playback time should remain 10.0, but was ${resultTime}")
  }

  test("When the player is PLAYING, playback time should advance according to deltaTime") {
    val currentTime = 10.0
    val deltaTime = 0.5

    val resultTime = TimelineEngine.updatePlaybackTime(
      currentTime,
      Playing(speed = 1.0),
      deltaTime,
      maxTimelineDuration
    )

    assert(resultTime == 10.5, s"Playback time should advance to 10.5, but was ${resultTime}")
  }

  test("When the player is PLAYING at 2x speed, playback time should advance at double the rate") {
    val currentTime = 10.0
    val deltaTime = 0.5

    val resultTime = TimelineEngine.updatePlaybackTime(
      currentTime,
      Playing(speed = 2.0),
      deltaTime,
      maxTimelineDuration
    )

    assert(resultTime == 11.0, s"At double speed it should be 11.0, but was ${resultTime}")
  }

  test("The playback cursor must NEVER exceed the maximum timeline duration") {
    val currentTime = 59.8
    val deltaTime = 0.5

    val resultTime = TimelineEngine.updatePlaybackTime(
      currentTime,
      Playing(speed = 1.0),
      deltaTime,
      maxTimelineDuration
    )

    assert(resultTime == 60.0, s"Playback time should clamp to 60.0, but was ${resultTime}")
  }