package clrs_book

import clrs_book.SumOnBinaryArrayStoredInt.curry

object SumOnBinaryArrayStoredInt extends App {

  // 7
  // 1000 0000  0000 0000  0000 0000 0000 0111  [4+2+1]
  val n1 = Array(1, 1, 1)


  // 3
  // 1000 0000  0000 0000  0000 0000 0000 0011 [2+1]
  val n2 = Array(0, 1, 1)

  // 7+3 = 10
  // 1000 0000  0000 0000  0000 0000 0000 1010 [8+2]

  /*
  Binary Sum Rules:
  0 + 1 = 1
  1 + 0 = 1
  1 + 1 = 0 [curry 1]
  1 + 1 + 1 = 1 [curry 1]
   */

  /*
  Loop iterations:
   i=0 -> 1+1 = 0, curry = 1
   i=1 -> 1+1+(curry-> 1) = 1, curry = 1
   i=2 -> 1+0+(curry-> 1) = 0, curry = 1
   i=3 -> 0+0+(curry-> 1) = 1
   */

  val n3 = Array(0, 0, 0, 0)
  var curry = 0

  for (i <- n1.length - 1 to(0, -1)) {
    val p2 = add(n1(i), n2(i), curry)
    n3(i + 1) = p2._1
    curry = p2._2

    println(n3.toIterable)
  }

  n3(0) = curry

  println(n3.toIterable)


  lazy val add: (Int, Int, Int) => (Int, Int) = (b1, b2, curry) =>
    (b1, b2, curry) match {
      case (1, 1, 1) => (1, 1)
      case (0, 0, 0) => (0, 0)

      case (1, 1, 0) => (0, 1)
      case (0, 0, 1) => (1, 0)

      case (1, 0, 0) => (1, 0)
      case (1, 0, 1) => (0, 1)

      case (0, 1, 1) => (0, 1)
      case (0, 1, 0) => (1, 0)
    }
}
