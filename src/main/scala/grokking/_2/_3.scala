package grokking._2

object _3 extends App {

  println(findMinSubArray(7, Array(2, 1, 5, 2, 3, 2)).toList)
  println(findMinSubArray(7, Array(2, 1, 5, 2, 8)).toList)
  println(findMinSubArray(8, Array(3, 4, 1, 1, 6)).toList)

  //sliding window pattern: slide = 1;
  // window length = start with 1 until we hit higher num than sum
  def findMinSubArray(resultSum: Int, arr: Array[Int]): Array[Int] = {

    var resultWindow = Int.MaxValue
    var resultStartIndex = -1

    var sum = 0
    var windowStart = 0
    var window = 0

    var i = 0
    while (i < arr.length) {
      window = window + 1
      sum = sum + arr(i)

      if (sum >= resultSum) {
        if (window < resultWindow) {
          resultStartIndex = windowStart
          resultWindow = window
        }
        windowStart = windowStart + 1
        window = 0
        sum = 0
        i = windowStart
      } else {
        i = i + 1
      }
    }

    (resultStartIndex until (resultStartIndex + resultWindow) map arr).toArray
  }

}
