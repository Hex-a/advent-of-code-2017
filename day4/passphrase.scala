val isP2 = args.size > 0 && args(0) == "-p2"

val r = 
  scala.io.Source.stdin.getLines
    .map   { _ split " " }
    .map   { x => if(isP2) x.map{_.sorted} else x }
    .count { x => x.distinct.size == x.size }

println(r)
