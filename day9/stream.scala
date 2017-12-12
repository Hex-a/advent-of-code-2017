case class State  (score: Int, groups: Int, ignoredChars: Int) { 
  def openGroup(): State = State(score, groups + 1, ignoredChars)
  def closeGroup(): State = State(score + groups, groups - 1, ignoredChars)
  def ignoreChar(): State = State(score, groups, ignoredChars + 1)
}

trait Mode {
  val st: State
  def next(c: Char): Mode
}

case class AcceptAnyMode(st: State) extends Mode {
  def next(c: Char) = c match {
    case '{' => AcceptAnyMode(st.openGroup)
    case '}' => AcceptAnyMode(st.closeGroup)
    case '!' => RejectNextMode(st, this)
    case '<' => GarbageMode(st)
    case _   => this
  }
}

case class RejectNextMode(st: State, prev: Mode) extends Mode {
  def next(c: Char) = prev
}

case class GarbageMode(st: State) extends Mode {
  def next(c: Char) = c match {
    case '!' => RejectNextMode(st, this)
    case '>' => AcceptAnyMode(st)
    case _   => GarbageMode(st.ignoreChar)
  }
}

object Mode {
  def apply(): Mode = AcceptAnyMode(State(0,0,0))
}

val finalSt = readLine().toArray
    .foldLeft(Mode()) { (st, c) => st.next(c) }
    .st

println(finalSt.score)
println(finalSt.ignoredChars)
