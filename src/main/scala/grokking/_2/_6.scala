package grokking._2


object _6 extends App {
  println(longestSubStringPostReplace("aabccbb", 2))
  println(longestSubStringPostReplace("aabccbb", 2) == 5)
  println(longestSubStringPostReplace("abbcb", 1))
  println(longestSubStringPostReplace("abbcb", 1) == 4)
  println(longestSubStringPostReplace("abccde", 1))
  println(longestSubStringPostReplace("abccde", 1) == 3)

  // to replace no more than ‘k’ letters with any letter,
  //  find the length of the longest substring having the same letters after replacement.
  // longest single char substring post replace
  // copied based
  def longestSubStringPostReplace(s: String, k: Int): Int = {
    import scala.collection.mutable
    val map = new mutable.HashMap[Char, Int]()
    var maxLength = 0

    var windowStart = 0
    0 until (s.length) foreach { windowEnd =>
      map.put(s(windowEnd), 1 + map.getOrElse(s(windowEnd), 0))
      val maxRepeatLetterCount = map.values.max
      println(s"Outer: $windowEnd - $windowStart; $maxRepeatLetterCount - $map")

      while (windowEnd - windowStart + 1 - maxRepeatLetterCount > k) {
        map.put(s(windowStart), map(s(windowStart)) - 1);
        windowStart = windowStart + 1
        println(s"Inner: ${windowEnd - windowStart}; $windowEnd - $windowStart; $map")
      }

      if (maxLength < (windowEnd - windowStart + 1)) maxLength = (windowEnd - windowStart + 1)
    }

    maxLength
  }

}
