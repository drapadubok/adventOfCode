
import scala.util.matching.Regex

case class Coords(x: Int, y: Int)

case class Command(exe: String, start: Coords, end: Coords)

def getCommand(line: String): Command = {
  // separate the groups: word number number word number number
  val pattern = """([\w\s]+)\s(\d+)[,](\d+)\s([\w\s]+)\s(\d+)[,](\d+)""".r
  val groups = pattern.findFirstMatchIn(line).get.subgroups
  Command(groups(0), Coords(groups(1).toInt, groups(2).toInt), Coords(groups(4).toInt, groups(5).toInt))
}

def through(start: Coords, end: Coords): Array[Array[Int]] = {
  val xs = (start.x to end.x)
  val ys = (start.y to end.y)
  Array(xs.toArray, ys.toArray)
}

def update(Grid: Array[Array[Boolean]], coords: Array[Array[Int]], exe: String): Array[Array[Boolean]] = {
  // Returns full Grid with elements in coords updates by exe
  for (x <- coords(0); y <- coords(1)) {
    exe match {
      case "toggle" => Grid(x)(y) = !Grid(x)(y)
      case "turn off" => Grid(x)(y) = false
      case "turn on" => Grid(x)(y) = true
      case _ => println("Unknown command: " + exe)
    }
  }
  Grid
}

val input = scala.io.Source.fromFile("advent06.txt").mkString.split("\n")
val Grid = Array.fill(1000, 1000)(false)
for (line <- input) {
  val cmd = getCommand(line)
  val coords = through(cmd.start, cmd.end)
  update(Grid, coords, cmd.exe)
}
val r1 = Grid.flatten.filter(identity).size
