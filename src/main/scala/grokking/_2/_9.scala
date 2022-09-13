package grokking._2

import scala.collection.mutable

object _9 extends App {

  println(findPermutation("oidbcaf", "abc"))
  println(findPermutation("odicf", "dc"))
  println(findPermutation("bcdxabcdy", "bcdyabcdx"))
  println(findPermutation("aaacb", "abc"))

  def findPermutation(str: String, pattern: String): Boolean = {

    val patternMap = pattern.groupBy(c => c).mapValues(_.length)

    def matched(c: Char, m: mutable.HashMap[Char, Int]) =
      m.getOrElse(c, 0) <= patternMap.getOrElse(c, -1)

    def bothMapSame(m: mutable.HashMap[Char, Int]): Boolean =
      m.size == patternMap.size &&
        patternMap.forall(t2 => m.getOrElse(t2._1, 0) == t2._2)

    val map = new mutable.HashMap[Char, Int]()
    0 until str.length foreach { i =>
      if (matched(str(i), map)) {
        map.put(str(i), map.getOrElse(str(i), 0) + 1)

        println(map)
        val b = bothMapSame(map)
        if (b) return b
      } else {
        map.clear
      }
    }
    false
  }


}
