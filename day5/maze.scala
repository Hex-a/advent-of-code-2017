import scala.io.Source.stdin

val cmds = stdin.getLines.map{_.toInt}.toBuffer
val isP2 = args.size > 0 && args(0) == "-p2"

var curLn = 0
var steps = 0
while (curLn < cmds.size) {
  val offset = cmds(curLn)
  cmds(curLn) += (if(isP2 && offset > 2) -1 else 1)
  curLn += offset
  steps += 1
}

println(steps)
