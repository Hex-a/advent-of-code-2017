def solve(reducer: (Int, Array[Int]) => Int): Int =
  scala.io.Source.stdin.getLines
       .map { _.split("\\s+").map(_.toInt) }
       .foldLeft(0)(reducer)

val result = 
  if(args.isEmpty || args(0) != "-p2") {
    // Solution for part 1
    solve { (acc, x) => acc + x.max - x.min }
  } else {
    // Solution for part 2
    solve { (acc, row) => 
      val x = 
        row.sorted.combinations(2) collectFirst { 
          case Array(a, b) if b % a == 0 => b / a 
        }
      acc + x.get
    }
  }

println(result)
