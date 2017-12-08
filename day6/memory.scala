val banks = readLine().split("\\s+").map{_.toInt}.toArray

def redistribute(s: Seq[Int]): Seq[Int] = {
  val i = s.indices maxBy s
  val v = s(i)
  val d = (v - 1) / s.size
  val r = ((v - 1) % s.size)
  val inRange = ((i + 1) to (i + r + 1)).map{_ % s.size}.toSet
  s.zipWithIndex.map { case (t, j) => 
    (if(j == i) 0 else t) + d + (if (inRange(j)) 1 else 0)
  }
}

val states: Stream[Seq[Int]] = banks #::
  (for(lastState <- states)
    yield redistribute(lastState))

var pastStates = Map[Seq[Int], Int]()
val (index, distance) = 
  states.zipWithIndex.map { case (st, i) =>
    val prevMem = pastStates
    pastStates += (st -> i)
    prevMem.get(st).map { j => (i, i - j) }
  }.collectFirst { case Some(t) => t }.get

println(index) // sol 1
println(distance) //sol 2
