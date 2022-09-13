package grokking._2

object _2 extends App {

  println(findMaxArray(3, Array(2, 1, 5, 1, 3, 2)).toList)
  println(findMaxArray_BurstForce(3, Array(2, 1, 5, 1, 3, 2)).toList)
  println(findMaxArray(2, Array(2, 3, 4, 1, 5)).toList)
  println(findMaxArray_BurstForce(2, Array(2, 3, 4, 1, 5)).toList)

  //burst force
  def findMaxArray_BurstForce(k: Int, arr: Array[Int]): Array[Int] = {
    var maxSum = 0
    var maxStart = -1

    0 until (arr.length - k) foreach { i =>
      val sum = (i until (i + k) map arr).sum
      if (sum > maxSum) {
        maxSum = sum
        maxStart = i
      }
    }

    maxStart until (k + maxStart) map (arr(_)) toArray
  }

  //sliding window pattern - window length = k, slide = 1
  def findMaxArray(k: Int, arr: Array[Int]): Array[Int] = {
    var maxArrayStart = -1
    var maxSum = -1

    var sum = 0
    var windowStart = 0
    var window = 0
    0 until arr.length foreach { i =>
      window = window + 1
      sum = sum + arr(i)

      if (window == k) {
        if (sum > maxSum) {
          maxSum = sum
          maxArrayStart = windowStart
        }

        window = window - 1
        sum = sum - arr(windowStart)
        windowStart = windowStart + 1
      }
    }

    (maxArrayStart until (maxArrayStart + k)).map(arr(_)).toArray
  }

}
