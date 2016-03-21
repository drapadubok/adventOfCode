class Present(lc: Int, wc: Int, hc: Int) {  
  val l: Int = lc
  val w: Int = wc
  val h: Int = hc
  
  val smallestSide = Array(l*w, w*h, h*l).min

  val surfaceArea = 2*l*w + 2*w*h + 2*h*l

  val totalPaperNeeded = smallestSide + surfaceArea

  override def toString(): String = "Present(" + lc + ", " + wc + ", " + hc + ")";
}

val input = scala.io.Source.fromFile("advent02.txt").mkString
val preprocessed = input.split("\n").map(_.split("x"))
val presents = preprocessed.map { x =>
  new Present( x(0).toInt, x(1).toInt, x(2).toInt )
}
val r1 = presents.foldLeft(0)(_ + _.totalPaperNeeded)

/* tests, when home create proper folder structure
val m = new Present(2,3,4)

class SetSpec extends FlatSpec {
  "A present with l==2, w==3, h==4" should "have surfaceArea 52" in {
    assert(m.surfaceArea(2,3,4) == 52)
  }
  "A present with l==2, w==3, h==4" should "have smallestSide 6" in {
    assert(m.smallestSide(2,3,4) == 6)
  }
}

*/
