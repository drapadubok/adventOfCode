
// My original iterative solution which is slow as hell
// But took about an hour to code
import scala.collection.mutable.{ArrayBuffer, Stack}

def passThroughLAS(line: Array[String]): Array[String] = {
  // The idea is just to check each character of the string
  var current = 0
  val stack = Stack[String]()
  val output = ArrayBuffer.empty[String]

  while (current < line.length) {
    val c = line(current)

    // if next character exists 
    if ( current + 1 < line.length ) {
      if ( c == line(current + 1) ) {
        // next character is the same - add to stack
        stack.push(c.toString)
      } else {
        // next character is not the same - add to output and clear the stack
        stack.push(c.toString)
        output += stack.length + stack.pop
        stack.clear
      }
    } else {
      stack.push(c.toString)
      output += stack.length + stack.pop
      stack.clear
    }
    current += 1
  }
  output.reduce(_+_).split("")
}

@scala.annotation.tailrec
def recExec(line: Array[String], c: Int): Array[String] = {
  if (c == 0) {
    line
  } else {
    recExec(passThroughLAS(line), c-1)
  }
}

val input = "1321131112".split("")
var i = 40
val output = recExec(input, i).length

/* It takes eternity for i = 50 */

// Solution with regex
def passThroughLAS(line: String): String = {
  // (\d) - match digit and remember the group
  // \1* - match zero or more instances of the previous group (* is to handle case of just single number)
  val re = """(\d)\1*""".r 
  // d in replaceAllIn is Match, check methods: http://www.scala-lang.org/api/rc2/scala/util/matching/Regex$$Match.html
  val outputLine = re.replaceAllIn(line, d => d.matched.size + d.matched.take(1))
  outputLine
}

var line = "1321131112"
var i = 50
@scala.annotation.tailrec
def recExec(line: String, c: Int): String = {
  if (c == 0) {
    line
  } else {
    recExec(passThroughLAS(line), c-1)
  }
}

val output = recExec(line, i).length
