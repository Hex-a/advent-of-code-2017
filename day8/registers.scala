import scala.io.Source.stdin;

val r = raw"(\w+) (\w+) (-?\d+) if (\w+) ([<>!=]=?) (-?\d+)".r

var m = Map[String, Int]()
var mx = 0

for(ln <- stdin.getLines) {
  ln match {
    case r(r1, op, v1, r2, pred, v2) =>  {
      val r2v = m.getOrElse(r2, 0)
      val shouldRun = pred match {
        case "<" => r2v < v2.toInt
        case ">" => r2v > v2.toInt
        case "==" => r2v == v2.toInt
        case "<=" => r2v <= v2.toInt
        case ">=" => r2v >= v2.toInt
        case "!=" => r2v != v2.toInt
      }
      if(shouldRun) {
        val cmd: (Int, Int) => Int = if(op == "inc") {_ + _} else { _ - _}
        m += r1 -> cmd(m.getOrElse(r1, 0), v1.toInt)
      }
      mx = math.max(mx, m.values.max)
    }
  }
}

println(mx)
