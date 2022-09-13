package grokking._2

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object _4_2 extends App {
  println(longestDistinctChars("araaci", 2))
  println(longestDistinctChars("araaci", 1))
  println(longestDistinctChars("cbbebi", 3))

  def longestDistinctChars(str: String, k: Int): List[String] = {

    val map = new mutable.HashMap[Char, Int]()

    def newCharAllowed(c: Char): Boolean =
      map.size < k || (map.size == k && map.contains(c))

    def removeFromMap(c: Char): Unit =
      if (map(c) == 1) map.remove(c) else map.put(c, map(c) - 1)


    var lastWindowEnd = 0
    val generated = new ListBuffer[String]()

    0 until str.length foreach { i =>

      var start = lastWindowEnd
      while (start < str.length && newCharAllowed(str(start))) {
        map.put(str(start), 1 + map.getOrElse(str(start), 0))
        start = start + 1
      }
      lastWindowEnd = start

      if (generated.isEmpty) {
        generated.append(str.substring(i, lastWindowEnd))
      } else if (generated.head.length < (lastWindowEnd - i)) {
        generated.clear
        generated.append(str.substring(i, lastWindowEnd))
      } else if (generated.head.length == (lastWindowEnd - i)) {
        generated.append(str.substring(i, lastWindowEnd))
      }

      removeFromMap(str(i))
    }

    generated.toList
  }

}
