

val isPart2 = args.size > 0 && args(0) == "part2"
val ln: Array[Int] = readLine().split("").map(_.toInt).toArray
val step = if (isPart2) ln.size/2 else 1
val result = ln.zipWithIndex
    .filter { case (c, i) => ln((i + step) % ln.size) == c }
    .foldLeft(0)(_+_._1)

println(result)
