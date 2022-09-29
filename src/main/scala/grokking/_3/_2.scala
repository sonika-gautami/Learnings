package grokking._3

import grokking.Utils

object _2 extends App {

  Utils.print(() =>
    indicesOfPairWithTargetSum(Array(1, 2, 3, 4, 6), 6),
    List(1, 3))
  Utils.print(() =>
    indicesOfPairWithTargetSum(Array(2, 5, 9, 11), 11),
    List(0, 2))

  //Given array is sorted
  def indicesOfPairWithTargetSum(arr: Array[Int], target: Int): List[Int] = {
    var left = 0
    var right = arr.length - 1

    while (left < right) {
      val sum = arr(left) + arr(right)
      if (sum == target) {
        return List(left, right)
      }
      else if (sum > target) {
        right = right - 1
      } else {
        left = left + 1
      }
    }

    Nil
  }
}
