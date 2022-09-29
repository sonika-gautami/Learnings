package grokking._3

import grokking.Utils

object _4 extends App {
  Utils.print(() =>
    squares(Array(-2, -1, 0, 2, 3)).toList,
    Array(0, 1, 4, 4, 9).toList
  )
  Utils.print(() =>
    squares(Array(-3, -1, 0, 1, 2)).toList,
    Array(0, 1, 1, 4, 9).toList
  )

  //given no are sorted (including -ve, 0, +ve
  //make squares - sort the squares
  def squares(arr: Array[Int]): Array[Int] = {
    //left pointer is where last -ve no is laying
    // right pointer is at first 0/+ve no is laying
    // then it ll work like Merge (of MergeSort)
    var right = 0
    while (arr(right) <= 0) {
      right = right + 1
    }
    var left = right - 1

    val squares = new Array[Int](arr.length)
    0 until arr.length foreach { i =>
      val leftS = if (left < 0) Int.MaxValue else arr(left) * arr(left)
      val rightS = if (right >= arr.length) Int.MaxValue else arr(right) * arr(right)

      if (leftS < rightS) {
        squares(i) = leftS
        left = left - 1
      } else {
        squares(i) = rightS
        right = right + 1
      }
    }

    squares
  }
}
