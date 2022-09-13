package grokking._2

import scala.collection.mutable

object _5 extends App {
  println(longestSubStrWithAllDistant("aabccbb"))
  println(longestSubStrWithAllDistant("aabccbb") == 3)
  println(longestSubStrWithAllDistant("abbbb"))
  println(longestSubStrWithAllDistant("abbbb") == 2)
  println(longestSubStrWithAllDistant("abccde"))
  println(longestSubStrWithAllDistant("abccde") == 3)
  println(longestSubStrWithAllDistant("dvdf"))
  println(longestSubStrWithAllDistant("dvdf") == 3)

  def longestSubStrWithAllDistant(s: String): Int = {

    val set = new mutable.HashSet[Char]()

    def newCharAllowed(c: Char) = !set.contains(c)

    def remove(c: Char): Unit = set.remove(c)

    def distinctCount: Int = set.size

    var max = -1
    var lastWindow = 0
    0 until s.length foreach { i =>

      var start = lastWindow
      while (start < s.length && newCharAllowed(s(start))) {
        set.add(s(start))
        start = start + 1
      }
      lastWindow = start

      if (max < distinctCount) max = distinctCount

      remove(s(i))
    }

    max
  }
}
