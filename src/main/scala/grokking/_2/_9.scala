package grokking._2

import scala.collection.mutable

object _9 extends App {

  println(findPermutation("oidbcaf", "abc"))
  println(findPermutation("odicf", "dc"))
  println(findPermutation("bcdxabcdy", "bcdyabcdx"))
  println(findPermutation("aaacb", "abc"))

  // it ll access elements more - algo will run in 0(n) without sliding-window
  def findPermutation_SimpleLoop_PatternHasNoRepeatChars(str: String, pattern: String): Boolean = {
    def hasAllChars(subStr: String): Boolean = {
      pattern.forall(c => subStr.contains(c))
    }

    0 to (str.length - pattern.length) exists { i =>
      hasAllChars(str.substring(i, i + pattern.length))
    }
  }

  //slice-window window length = pattern.length
  def findPermutation_2maps(s2: String, s1: String): Boolean = {
    val patternMap = s1.groupBy(c => c).mapValues(_.length)

    val windowMap = new mutable.HashMap[Char, Int]()

    def bothMapSame: Boolean =
      patternMap.size == windowMap.size &&
        patternMap.forall(t2 => windowMap.getOrElse(t2._1, 0) == t2._2)

    def put(c: Char): Unit = windowMap.put(c, windowMap.getOrElse(c, 0) + 1)

    def remove(c: Char): Unit = {
      if (windowMap(c) == 1) windowMap.remove(c)
      else windowMap.put(c, windowMap(c) - 1)
    }

    val window = s1.length
    var windowStart = 0
    0 until s2.length foreach { windowEnd =>
      put(s2(windowEnd))

      if ((windowEnd - windowStart + 1) == window) {
        val b = bothMapSame
        if (b) return true

        remove(s2(windowStart))
        windowStart = windowStart + 1
      }
    }
    false
  }

  def findPermutation(str: String, pattern: String): Boolean = {
    import scala.collection.mutable
    val map = new mutable.HashMap[Char, Int]()
    map.++=(pattern.groupBy(c => c).mapValues(_.length))
    var charWithZeroCount = 0

    def remove(c: Char): Unit = if (map.contains(c)) {
      map.put(c, map(c) - 1)
      if (map(c) == 0) charWithZeroCount = charWithZeroCount + 1
    }

    def add(c: Char): Unit = if (map.contains(c)) {
      if (map(c) == 0) charWithZeroCount = charWithZeroCount - 1
      map.put(c, map.getOrElse(c, 0) + 1)
    }

    val window = pattern.length
    var windowStart = 0
    0 until str.length foreach { windowEnd =>
      remove(str(windowEnd))

      if ((windowEnd - windowStart + 1) == window) {
        //if (map.values.forall(_ == 0)) return true
        //to avoid this, let's use one variable which maintains count of zero char
        //map.size = pattern's distinct char size
        if (charWithZeroCount == map.size) return true

        add(str(windowStart))
        windowStart = windowStart + 1
      }
    }
    false
  }
}
