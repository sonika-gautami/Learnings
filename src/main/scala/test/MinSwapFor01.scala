package test

object MinSwapFor01 extends App {

  println(minSwaps(Array(0, 1, 0, 1, 1, 0, 0)))
  println(minSwapsCircularArray(Array(1, 1, 1, 1, 0, 1, 1)))

  def minSwaps(nums: Array[Int]): Int = {

    val window = nums.filter(_ == 1).size
    var windowStart = 0
    var maxSwap = Int.MaxValue
    var currWindowCount = 0
    0 until nums.length foreach { windowEnd =>
      if (nums(windowEnd) == 0) currWindowCount = currWindowCount + 1

      if (windowEnd - windowStart + 1 == window) {
        if (maxSwap > currWindowCount) maxSwap = currWindowCount

        if (nums(windowStart) == 0) currWindowCount = currWindowCount - 1
        windowStart = windowStart + 1
      }
    }

    if (maxSwap == Int.MaxValue) 0 else maxSwap
  }

  def minSwapsCircularArray(nums: Array[Int]): Int = {

    val window = nums.filter(_ == 1).size
    var windowStart = 0
    var maxSwap = Int.MaxValue
    var currWindowCount = 0
    0 until (nums.length + window - 1) foreach { windowEnd_ =>
      val windowEnd = windowEnd_ % nums.length

      if (nums(windowEnd) == 0) currWindowCount = currWindowCount + 1

      if (windowEnd_ - windowStart + 1 == window) {
        if (maxSwap > currWindowCount) maxSwap = currWindowCount

        if (nums(windowStart) == 0) currWindowCount = currWindowCount - 1
        windowStart = windowStart + 1
      }
    }

    if (maxSwap == Int.MaxValue) 0 else maxSwap
  }

}
