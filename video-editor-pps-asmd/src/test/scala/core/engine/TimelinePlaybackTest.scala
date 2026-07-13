package core.engine

import org.scalatest.funsuite.AnyFunSuite
import core.model.*

class TimelinePlaybackTest extends AnyFunSuite:

  val maxTimelineDuration = 60.0

  test("Se il player è in PAUSA, il tempo non deve avanzare anche se passa il tempo reale") {
    val currentTime = 10.0
    val deltaTime = 0.5

    val resultTime = TimelineEngine.updatePlaybackTime(
      currentTime,
      Paused,
      deltaTime,
      maxTimelineDuration
    )

    assert(resultTime == 10.0, s"Il tempo doveva rimanere 10.0, invece è ${resultTime}")
  }

  test("Se il player è in PLAYING, il tempo deve avanzare in base al deltaTime") {
    val currentTime = 10.0
    val deltaTime = 0.5

    val resultTime = TimelineEngine.updatePlaybackTime(
      currentTime,
      Playing(speed = 1.0),
      deltaTime,
      maxTimelineDuration
    )

    assert(resultTime == 10.5, s"Il tempo doveva avanzare a 10.5, invece è ${resultTime}")
  }

  test("Se il player è in PLAYING con velocità 2x, il tempo deve avanzare del doppio") {
    val currentTime = 10.0
    val deltaTime = 0.5

    val resultTime = TimelineEngine.updatePlaybackTime(
      currentTime,
      Playing(speed = 2.0),
      deltaTime,
      maxTimelineDuration
    )

    assert(resultTime == 11.0, s"Al doppio della velocità doveva essere 11.0, invece è ${resultTime}")
  }

  test("Il cursore del tempo non deve MAI superare la durata massima della timeline") {
    val currentTime = 59.8
    val deltaTime = 0.5 

    val resultTime = TimelineEngine.updatePlaybackTime(
      currentTime,
      Playing(speed = 1.0),
      deltaTime,
      maxTimelineDuration
    )

    assert(resultTime == 60.0, s"Il tempo doveva bloccarsi a 60.0, invece è andato a ${resultTime}")
  }