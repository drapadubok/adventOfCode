import scala.annotation.tailrec

def md5(s: String) = {
  import java.security.MessageDigest
  MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
}

def lineOfZeros(i: Int): String = {
  "0"*i
}

@tailrec
def recursiveScan(s: String, num: Int, zeros: Int): Int = {
  val firstFiveNumbersHash = md5(s + num).take(zeros)
  val temp = firstFiveNumbersHash == lineOfZeros(zeros)
  
  temp match {
    case true => num
    case false => recursiveScan(s, num+1, zeros)
  }
}

val input = "ckczppom"
val output1 = recursiveScan(input, 0, 5)
val output2 = recursiveScan(input, 0, 6)
