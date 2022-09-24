package grokking._2

object _6_2 extends App {
  println(longestSubStringPostReplace("aabccbb", 2))
  println(longestSubStringPostReplace("aabccbb", 2) == 5)
  println(longestSubStringPostReplace("abbcb", 1))
  println(longestSubStringPostReplace("abbcb", 1) == 4)
  println(longestSubStringPostReplace("abccde", 1))
  println(longestSubStringPostReplace("abccde", 1) == 3)

  // to replace no more than ‘k’ letters with any letter,
  //  find the length of the longest substring having the same letters after replacement.
  // longest single char substring post replace
  def longestSubStringPostReplace(s: String, k: Int): Int = {
    import scala.collection.mutable
    val map = new mutable.HashMap[Char, Int]()

    def putInMap(c: Char, dir: Int = 1): Unit =
      map.put(c, map.getOrElse(c, 0) + (1 * dir))

    def maxCommonCharsCountFromMap: Int = map.values.max

    var maxLength = 0
    var windowStart = 0
    var maxCommonCharsCountWithinWindow = 0


    // sliding-window:
    // window =  k (can be replaceable) + max-common-chars length
    // when our loop-window crosses this expected window,
    //    we got the string-length with same chars after k replaces
    //    we now move our windowStart to increase & we remove from map the last-window entry
    0 until s.length foreach { windowEnd =>
      putInMap(s(windowEnd))

      if (maxCommonCharsCountWithinWindow < map(s(windowEnd)))
        maxCommonCharsCountWithinWindow = map(s(windowEnd))

      val windowLength = k + maxCommonCharsCountWithinWindow
      val currWindowLength = windowEnd - windowStart + 1
      println(s"$currWindowLength; $windowLength; $map")

      if (currWindowLength > windowLength) {
        putInMap(s(windowStart), -1)
        maxCommonCharsCountWithinWindow = maxCommonCharsCountFromMap
        windowStart = windowStart + 1

        // -1 because the curr-char caused the expected-window to cross
        //    windowEnd char is neither the same-char-with-max-length nor it's < k (max replaceable)
        if (maxLength < (currWindowLength - 1)) maxLength = currWindowLength - 1
      } else {
        // when curr char at windowEnd is inclusive
        if (maxLength < (currWindowLength)) maxLength = currWindowLength
      }

    }

    maxLength
  }

}
