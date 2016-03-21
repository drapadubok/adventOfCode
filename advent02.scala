def twoSmallestElements(a: Array[Int]): Array[Int] = {
  a.sorted.take(2)
}

def smallestPerimeter(l: Int, w: Int, h: Int): Int = {
  twoSmallestElements(Array(l, w, h)).foldLeft(0)(_+_) * 2
}

class Present(lc: Int, wc: Int, hc: Int) {  
  val l: Int = lc
  val w: Int = wc
  val h: Int = hc
  // First star
  val smallestSide = Array(l*w, w*h, h*l).min
  val surfaceArea = 2*l*w + 2*w*h + 2*h*l
  val totalPaperNeeded = smallestSide + surfaceArea
  // Second star
  val ribbonWrap = smallestPerimeter(l, w, h)
  val ribbonBow = l * w* h
  val ribbonTotal = ribbonWrap + ribbonBow
  
  override def toString(): String = "Present(" + lc + ", " + wc + ", " + hc + ")";
}


val input = scala.io.Source.fromFile("advent02.txt").mkString
val preprocessed = input.split("\n").map(_.split("x"))
val presents = preprocessed.map { x =>
  new Present( x(0).toInt, x(1).toInt, x(2).toInt )
}
val r1 = presents.foldLeft(0)(_ + _.totalPaperNeeded)
val r2 = presents.foldLeft(0)(_ + _.ribbonTotal)

/* tests, when home create proper folder structure
val m = new Present(2,3,4)

class SetSpec extends FlatSpec {
  "A present with l==2, w==3, h==4" should "have surfaceArea 52" in {
    assert(m.surfaceArea == 52)
  }
  "A present with l==2, w==3, h==4" should "have smallestSide 6" in {
    assert(m.smallestSide == 6)
  }
  "A present with l==2, w==3, h==4" should "have ribbonWrap 10" in {
    assert(m.ribbonWrap == 10)
  }
  "A present with l==2, w==3, h==4" should "have ribbonBow 24" in {
    assert(m.ribbonBow == 24)
  }
}
*/
