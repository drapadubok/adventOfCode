
val input = scala.io.Source.fromFile("advent03.txt").mkString.split("")

// http://stackoverflow.com/questions/19606802/add-two-tuples-constaining-simple-elements-in-scala
implicit class TupleAdd[A : Numeric, B : Numeric](t: (A, B)) {
  import Numeric.Implicits._
  def + (p: (A, B)) = (p._1 + t._1, p._2 + t._2)
}

// Map steps to coordinates
val steps = input.map { 
  case "^" => (0,1)
  case ">" => (1,0)
  case "v" => (0,-1)
  case "<" => (-1,0)
}

// First star
val path = steps.scanLeft((0,0))(_+_)
val visitedLocations = path.groupBy { x => x } // groupBy identity
val r1 = visitedLocations.size // we grouped all unique locations Santa visited, just count

// Second star, Santa1 takes even steps, Santa2 takes odd steps
val partitionedSteps = steps.zipWithIndex.partition( x => x._2%2==0 )

val santa1path = partitionedSteps._1.map(x => x._1).scanLeft((0,0))(_+_)
val santa2path = partitionedSteps._2.map(x => x._1).scanLeft((0,0))(_+_)

val combinedVisited = santa1path ++ santa2path
val uniqueCombinedVisited = combinedVisited.groupBy {x => x}
val r2 = uniqueCombinedVisited.size

/* add test cases: 
val input = "^v".split("")
val input = "^>v<".split("")
val input = "^v^v^v^v^v".split("")
*/
