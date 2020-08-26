package clrs_book

object InsertionSort extends App {

  /*

  8 n^2 > 64 n log-n
  n > 8 log-n
  n/8 > log-n

    As, log n of base b = x =>  n = b^x

  2^ n/8 > n

Lets say
n  = 8 -> 2 > 8
n = 16 -> 4 < 16
n = 24 -> 8 < 24
n = 32 -> 16 < 32
n = 40 -> 32 < 40
n = 48 -> 64 < 48     <-- Crossed  Between 40 & 48
n = 56 -> 128 < 56


100 n^2  < 2^n

Lets say
n = 1 100 < 2
n = 7 100 * 49 < 128
n = 50 100* 2500 < 2^10 * 5


   */

  val a = Array(5, 7, 6, 1, 8, 3, 10, 2, 0, 4, 9)
  println(s"Input:  ${a.toIterable}")


  import scala.util.control.Breaks._

  for (j <- 1 until a.length) {
    breakable {
      for (i <- (j - 1) to(0, -1)) {
        println(s"i: $i")

        if (a(i) < a(i + 1)) break;

        val key = a(i + 1)
        a(i + 1) = a(i)
        a(i) = key
        println(s"output: ${a.toIterable}")
      }
    }
  }

  println(s"Output: ${a.toIterable}")


  //Linear Search:
  //Worst Case -> element is at last -> n times
  //Best Case -> element is at first -> Constant Time
  //Avg Case -> n/2 -> ~ n times
  val valueToLookFor = 10
  var index: Option[Int] = None
  breakable {
    for (i <- 0 until a.length) {
      if (a(i) == valueToLookFor) {
        index = Some(i)
        break()
      }
    }
  }
  println(s"$valueToLookFor avialble at: ${index}")


}
