// This is a tough one, even though the solution was quite clear, had to suffer a lot to get to the point when it worked
// My key problems: 
// 1) didn't read collection part of documentation, hence took time to find out the way to use Map correctly
// 2) didn't consider at first that values need to be memoized, ended up with infinite loop
// 3) Didn't come up with case Array(a, "blah", b) expression, had to use quite verbose approach. Again, pattern matching is awesome!
// 4) Was running into problems with LSHIFT/RSHIFT Int, since my integer check was in the wrong place.

val input = scala.io.Source.fromFile("/m/nbe/home/smirnod1/advent07.txt").mkString.split("\n")
// construct dictionary of output: expression
val exprDict = input.map { line =>
  val temp = line.split(" -> ")
  (temp(1) -> temp(0))
}.toMap

// Memoize: when we reach repeated wires, it has to know the value of the repeated wire, otherwise we are in infinite loop
var resultDict = Map.empty[String, Int]

def solve(out: String): Int = {
  // try to get the result from memoized values
  resultDict.getOrElse(out, {
    val n = if (out(0).isDigit) {
      // if we got integer - return this integer
      out.toInt
    } else {
      // check which expression generated this output
      val expr = exprDict(out)
      val ops = expr.split(" ")
      ops match {
        case Array(a) => solve(a) 
        case Array(a,"AND",b) => solve(a) & solve(b)
        case Array(a,"OR",b) => solve(a) | solve(b)
        case Array(a,"LSHIFT",b) => solve(a) << solve(b)
        case Array(a,"RSHIFT",b) => solve(a) >> solve(b)
        case Array("NOT",b) => ~solve(b) & 0xffff // thanks to the internetz     
      }
    }
    // memoize and return result
    resultDict += (out -> n)
    n
  })
}

solve("a")
