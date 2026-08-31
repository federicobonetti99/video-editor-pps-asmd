package core.model

case class History[A](
                       past: List[A] = Nil,
                       current: A,
                       future: List[A] = Nil
                     ):

  def push(next: A): History[A] =
    if current == next then this
    else History(past = current :: past, current = next, future = Nil)

  def canUndo: Boolean = past.nonEmpty

  def canRedo: Boolean = future.nonEmpty

  def undo(): History[A] = past match
    case head :: tail =>
      History(past = tail, current = head, future = current :: future)
    case Nil => this

  def redo(): History[A] = future match
    case head :: tail =>
      History(past = current :: past, current = head, future = tail)
    case Nil => this