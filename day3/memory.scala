implicit class TupleImplicits(t: (Int, Int)) {
  def +(p: (Int, Int)) = (p._1 + t._1, p._2 + t._2)
}

val moves = 
  for(n <- Stream.from(3, 2);
      (v, n) <- Stream((0,1),(-1,0),(0,-1),(1,0)) zip List(n, n+1, n+1, n+2);
      v2 <- Stream.continually(v) take n)
  yield v2

val initialPos = (2, -1)
val cells = moves.scanLeft(initialPos){_+_}

// Part 1

val (x, y) = (cells(347991 - 10))
println(x.abs + y.abs)

// Part 2

var m = Map[(Int, Int), Int](
  (1,0) -> 1,
  (1,1) -> 2,
  (0,1) -> 4,
  (-1,1) -> 5,
  (-1,0) -> 10,
  (-1,-1) -> 11,
  (0, -1) -> 23,
  (1, -1) -> 25
)

val cs = List(-1, 0, 1)
val cross = 
  for(x <- cs;
      y <- cs) yield (x, y)

var r = 
  cells.map { t  => 
    val v = cross.map{ x: (Int, Int) => m.getOrElse(t + x, 0) }.sum
    m += t -> v
    v
  }

println(r.find(_ > 347991).get)
