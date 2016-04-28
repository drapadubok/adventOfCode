
val input = scala.io.Source.fromFile("/m/nbe/home/smirnod1/advent09.txt").mkString.split("\n")

// Extract only needed data: Array(from, to, distance)
val mappedInput = input.map { item =>
  val itemSplit = item.split(" ")
  Array(itemSplit(0), itemSplit(2), itemSplit(4))
}

// convert string names to integers, utility for adjacency matrix
val uniqueNames = mappedInput.map( i => Array(i(0),i(1)) ).flatten.toSet.zipWithIndex.toMap
val mappedInputN = mappedInput.map { i =>
  Array(uniqueNames(i(0)), uniqueNames(i(1)), i(2).toInt)
}

// create adjacency matrix, fill it in with corresponding values
val ndim = uniqueNames.size
val AdjacencyMatrix = Array.fill(ndim, ndim)(0)
for (i <- mappedInputN) {
  AdjacencyMatrix(i(0))(i(1)) = i(2)
}

// Random permutation of sequence of numbers (0 until n)
def permuteRange(n: Int): Array[Int] = scala.util.Random.shuffle(0 to n-1).toArray

// We're data scientists here, so instead of solving TSP, I'm going with Monte-Carlo approach
// Generate multiple permutations of possible routes, check distance, and return the shortest
// While not guaranteed to be the best, at least guaranteed to NOT be the worst
def calculateRouteAndDistance(nperm: Int, ndim: Int): Array[Any] = {
  var c = 0
  var dist = 99999

  while (c < nperm) {
    val route = permuteRange(ndim)
    val routeDist = route.sliding(2).toArray.map { i =>
      // If 0, look at other triangle
      if (AdjacencyMatrix(i(0))(i(1)) == 0) {
        AdjacencyMatrix(i(1))(i(0))
      } else {
        AdjacencyMatrix(i(0))(i(1))
      }
    }
    if (routeDist.sum < dist) {
      dist = routeDist.sum
      val returnRoute = route
    }
    c += 1
  }
  Array(dist, returnRoute)
}

val nperm = 1000000
val w = calculateRouteAndDistance(nperm, ndim)
val r1 = w(0)
