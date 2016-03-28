import scala.util.matching.Regex

val input = scala.io.Source.fromFile("advent05.txt").mkString.split("\n")

// Using regex
val patternVowels = new Regex("[aeiou]")
val patternLetterTwice = new Regex("([a-z])\\1+")
val patternExceptions = new Regex("(ab|cd|pq|xy)")

def isNotNaughty(s: String): Boolean = {
  val t1 = patternVowels.findAllIn(s).size >= 3
  val t2 = patternLetterTwice.findFirstIn(s).size >= 1
  val t3 = patternExceptions.findFirstIn(s).size == 0
  t1 & t2 & t3  
}

val r1 = input.filter(isNotNaughty(_)).size

// For second star
def hasNonoverlappingPair(line: String): Boolean = {
// If line has pairs that are distanced by more than 1 character
  line.sliding(2).toList.exists{ c =>
    line.lastIndexOf(c) - line.indexOf(c) > 1
  }
}

def sandwiched(line: String): Boolean = {
  line.sliding(3).toList.exists{ t =>
    t(0) == t(2)
  }
}

val r2 = input.filter { line => 
  sandwiched(line) & hasNonoverlappingPair(line)
}.length

/* some wasted effort, disregard
def containsNumVowels(s: String, num: Int): Boolean = {
  require (num > 0, "Num should be larger than zero, now: " + num)  
  val vowels = "aeiou"
  s.filter(vowels.indexOf(_) >= 0).size >= num
}*/
