package core.model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HistoryTest extends AnyFunSuite with Matchers:

  test("Initial history has no undo and no redo"):
    val history = History(current = "State 1")
    history.current shouldBe "State 1"
    history.canUndo shouldBe false
    history.canRedo shouldBe false
    history.undo() shouldBe history
    history.redo() shouldBe history

  test("Pushing a new state updates current, enables undo, and clears future"):
    val h0 = History(current = "State 1")
    val h1 = h0.push("State 2")

    h1.current shouldBe "State 2"
    h1.past shouldBe List("State 1")
    h1.future shouldBe empty
    h1.canUndo shouldBe true
    h1.canRedo shouldBe false

  test("Undo restores previous state and enables redo"):
    val h0 = History(current = "State 1").push("State 2")
    val undone = h0.undo()

    undone.current shouldBe "State 1"
    undone.past shouldBe empty
    undone.future shouldBe List("State 2")
    undone.canUndo shouldBe false
    undone.canRedo shouldBe true

  test("Redo restores undone state"):
    val h0 = History(current = "State 1").push("State 2")
    val undone = h0.undo()
    val redone = undone.redo()

    redone.current shouldBe "State 2"
    redone.past shouldBe List("State 1")
    redone.future shouldBe empty
    redone.canUndo shouldBe true
    redone.canRedo shouldBe false

  test("Multiple undos and redos maintain correct sequence"):
    val h = History(current = 1).push(2).push(3).push(4)

    h.current shouldBe 4

    val u1 = h.undo()
    u1.current shouldBe 3

    val u2 = u1.undo()
    u2.current shouldBe 2

    val u3 = u2.undo()
    u3.current shouldBe 1
    u3.canUndo shouldBe false

    val r1 = u3.redo()
    r1.current shouldBe 2

    val r2 = r1.redo()
    r2.current shouldBe 3

  test("Pushing identical state does not alter history"):
    val h = History(current = "A")
    val unchanged = h.push("A")
    unchanged shouldBe h

  test("Pushing a new state after undo drops any remaining future branches"):
    val h = History(current = "A").push("B").push("C")
    val undone = h.undo()

    val branched = undone.push("D")
    branched.current shouldBe "D"
    branched.past shouldBe List("B", "A")
    branched.future shouldBe empty
    branched.canRedo shouldBe false