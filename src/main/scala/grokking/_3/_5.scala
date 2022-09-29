package grokking._3

import scala.collection.mutable.ArrayBuffer

object _5 extends App {
  grokking.Utils.print(() =>
    allTripletsSumToZero(Array(-3, 0, 1, 2, -1, 1, -2))
      .map(_.toList).toList,
    List(List(-3, 1, 2), List(-2, 0, 2), List(-2, 1, 1), List(-1, 0, 1))
  )
  grokking.Utils.print(() =>
    allTripletsSumToZero(Array(-5, 2, -1, -2, 3))
      .map(_.toList).toList,
    List(List(-5, 2, 3), List(-2, -1, 3))
  )

  def allTripletsSumToZero(arr: Array[Int]): Array[Array[Int]] = {
    /*
     x + y + z = 0
     y + z = -x

     Sort the given array
      Apply pairs with sum x instead zero
        need to skip duplicates
     */

    val a = arr.sorted
    val triplets = new ArrayBuffer[Array[Int]]()

    targetSum(a(0), 1 * -1)
    1 until (a.length - 2) foreach { i =>
      if (a(i) != a(i - 1)) targetSum(a(i), (i + 1) * -1)
    }

    def targetSum(target: Int, left: Int): Unit = {
      var right = a.length
      while ()

    }

    triplets.toArray
  }

  //burst-force - returns with duplicates
  def allTripletsSumToZeroBurstForce(arr: Array[Int]): Array[Array[Int]] = {
    val triplets = new ArrayBuffer[Array[Int]]()

    def loop(n: Int, index: Int, acc: List[Int], accSum: Int): Unit = {
      if (n > 2) {
        if (accSum == 0) triplets.append(acc.toArray)
      } else {
        (index + 1) until arr.length foreach { i =>
          loop(n + 1, i, acc :+ (arr(i)), accSum + arr(i))
        }
      }
    }

    0 until arr.length foreach { i =>
      loop(1, i, List(arr(i)), arr(i))
    }
    triplets.toArray
  }
}
