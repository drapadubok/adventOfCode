val input = scala.io.Source.fromFile("advent01.txt").mkString

def isMinusOne (i : Int): Boolean = if (i == (-1)) true else false

val result = input.map { i =>
  i.toString match {
    case "(" => 1
    case ")" => -1
  }
}

val r1 = result.foldLeft(0)(_+_)
val r2 = result.scanLeft(0)(_+_).zipWithIndex.filter( x => isMinusOne(x._1) )
