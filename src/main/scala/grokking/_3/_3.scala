package grokking._3

import grokking.Utils

object _3 extends App {
  Utils.print(() =>
    removeDuplicatesNoExtraSpace(Array(2, 3, 3, 3, 6, 9, 9)),
    4
  )
  Utils.print(() =>
    removeDuplicatesNoExtraSpace(Array(2, 2, 2, 11)),
    2
  )
  Utils.print(() =>
    removeDuplicatesNoExtraSpace(Array(1, 2, 3, 11)),
    4
  )

  //sorted array + return count of unique
  //returning back the length of array till which uniques are stored
  def removeDuplicatesNoExtraSpace(arr: Array[Int]): Int = {
    if (arr.length < 2) arr.length
    else {
      var left = 0
      var right = 1

      while (right < arr.length) {
        if (arr(left) != arr(right)) {
          left = left + 1

          arr(left) = arr(right)
        }
        right = right + 1
      }

      println(arr.toList)
      left + 1
    }
  }

  //working; only gives count
  def removeDuplicates(arr: Array[Int]): Int = {
    if (arr.length < 2) arr.length
    else {
      var left = 0
      var right = 1
      var distinct = 0
      while (right < arr.length) {
        if (arr(left) != arr(right)) {
          distinct = distinct + 1
          left = right
        }
        right = right + 1
      }
      distinct = distinct + 1

      distinct
    }
  }
}
