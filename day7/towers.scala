import scala.io.Source.stdin;


case class Program (name: String, weight: Int, subtowers: Seq[Program]) {
  lazy val totalWeight: Int = weight + subtowers.map{_.totalWeight}.sum
  def supports(prog: Program): Boolean = subtowers contains prog
}

object Program {
  private val program = raw"(\w+) \((\d+)\)(?: -> (.*))?".r

  def parse(s: Seq[String]): Program = {
    val graph = s.collect {
      case program(n, w, null) => (n -> (w.toInt, Seq[String]()))
      case program(n, w, deps) => 
        val xs = deps.split(",").map{ _.trim }.toSeq
        (n -> (w.toInt, xs))
    }.toMap

    val programs = scala.collection.mutable.Map[String, Program]()
    lazy val mkProg: String => Program = 
      { x => 
        programs.getOrElseUpdate(x,{
          val (w, xs) = graph(x)
          Program(x, w, xs.map{mkProg(_)})
        })
      }
    graph.keys.foreach(mkProg)

    programs.values.find { k => !programs.values.exists(_ supports k) }.get
  }
}

def solve(p: Program): Option[Int] = {
  val ts = p.subtowers
  val r = ts.foldLeft(None: Option[Int]){ (x, y) => x.orElse(solve(y)) }
  r orElse {
    val x = ts.groupBy(_.totalWeight).toSeq.sortWith(_._2.size < _._2.size).map{_._2(0)}
    if(x.size > 1)
      Some(x(0).weight - (x(0).totalWeight - x(1).totalWeight))
    else
      None
  }
}

val bottom = Program.parse(stdin.getLines.toSeq)

println(bottom.name) //sol1 
println(solve(bottom)) // sol2
