import scala.annotation.tailrec

def md5(s: String) = {
  import java.security.MessageDigest
  MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
}

@tailrec
def recursiveScan(s: String, num: Int): Int = {
  val firstFiveNumbersHash = md5(s + num).take(5)
  val temp = firstFiveNumbersHash == "00000"
  
  temp match {
    case true => num
    case false => recursiveScan(s,num+1)
  }
}


val input = "ckczppom"
val output = recursiveScan(input, 0)
