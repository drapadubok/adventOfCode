
def addEnclosingQuotes(line: String): String = {
  "\"" + line + "\""
}

def dropEnclosingQuotes(line: String): String = {
  line.drop(1).dropRight(1)
}

// handle tuple summation
implicit class TupleAdd[A : Numeric, B : Numeric](t: (A, B)) {
  import Numeric.Implicits._
  def + (p: (A, B)) = (p._1 + t._1, p._2 + t._2)
}

def scanString(line: String): (Int, Int) = {
  // we move one char at a time and inspect if it is backlash
  var current = 0
  var codeRep = 2 // start at 2 because we already accounted for enclosing quotes
  var stringRep = 0

  while (current < line.length) {
    val c = line(current)
    // if backslash, there are just two possible options
    if ("\\".contains(c)) {
      // check the next character to know which option we deal with
      current += 1
      line(current).toString match {
        case "x" => {
          // if this is hex escaping, skip next next two characters, and update the containers
          current += 3
          codeRep += 4
          stringRep += 1
        }
        case _ => {        
          current += 1
          codeRep += 2
          stringRep += 1          
        }
      }
    } else {
      codeRep += 1
      stringRep += 1
      current += 1 
    }
  }
  (codeRep, stringRep)
}

def convertString(line: String): String = {
  /*// Procedural style
  var current = 0
  var stack = ""
  while (current < line.length) {
    line(current).toString match {
      case "\"" => stack += """\""""
      case "\\" => stack += """\\"""
      case c => stack += c
    }
    current += 1
  }
  */
  // functional style
  line.split("").map {
    case "\"" => """\""""
    case "\\" => """\\"""
    case c => c
  }.mkString
}

val input = scala.io.Source.fromFile("/m/nbe/home/smirnod1/advent08.txt").mkString.split("\n")
val noEnclosingQuotes = input.map(dropEnclosingQuotes)
val result = noEnclosingQuotes.map(scanString).foldLeft((0,0))(_+_)
var r1 = result._1 - result._2

val newInput = input.map(convertString)
val result = newInput.map(scanString).foldLeft((0,0))(_+_)
var r2 = result._1 - result._2





