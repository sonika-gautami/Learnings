package grokking._2

import grokking.Utils

object _8 extends App {
  Utils.print(() =>
    maxLongest1sPostReplace(Array(0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1), k = 2),
    6
  )
  Utils.print(() =>
    maxLongest1sPostReplace(Array(0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1), k = 3),
    9
  )

  def maxLongest1sPostReplace(arr: Array[Int], k: Int): Int = {
    // 01100011011
    // windows: (for each the loop iteration)
    // 0110  4
    //  1100 4
    //   100 3
    //    00 2
    //     00 2
    //      011011 6
    /* Algorithm: slide-window:
          Move windowEnd until we cross window;
          when we cross window increment windowStart
          Do this till we hit the last element
     */
    var windowStart = 0
    var max1sLength = 0
    var countOf1s = 0

    0 until arr.length foreach { windowEnd =>
      if (arr(windowEnd) == 1) countOf1s = countOf1s + 1

      val currentWindow = windowEnd - windowStart + 1
      val expectedWindow = countOf1s + k
      if (currentWindow > expectedWindow) {
        if (max1sLength < currentWindow - 1) max1sLength = currentWindow - 1

        if (arr(windowStart) == 1) countOf1s = countOf1s - 1
        windowStart = windowStart + 1
      }
    }
    if (max1sLength < (arr.length - windowStart)) max1sLength = (arr.length - windowStart)

    max1sLength
  }
}
