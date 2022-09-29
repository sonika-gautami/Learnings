package grokking._2

import grokking.Utils


object _11 extends App {
  Utils.print(() =>
    findAllPermutationsWithStartingIndex("ppqp", "pq").toSeq,
    Seq(1, 2)
  )
  Utils.print(() =>
    findAllPermutationsWithStartingIndex("abbcabc", "abc").toSeq,
    Seq(2, 3, 4)
  )


  def findAllPermutationsWithStartingIndex(s: String, p: String): Array[Int] = {
    import scala.collection.mutable
    val map = new mutable.HashMap[Char, Int]()
    map.++=(p.groupBy(c => c).mapValues(_.length))

    import scala.collection.mutable.ArrayBuffer
    val results = new ArrayBuffer[Int]()

    var countOfZerosInPatternMap = 0

    def put(c: Char): Unit = if (map.contains(c)) {
      if (map(c) == 0) countOfZerosInPatternMap = countOfZerosInPatternMap - 1
      map.put(c, map(c) + 1)
    }

    def remove(c: Char): Unit = if (map.contains(c)) {
      map.put(c, map(c) - 1)
      if (map(c) == 0) countOfZerosInPatternMap = countOfZerosInPatternMap + 1
    }

    val window = p.length
    var windowStart = 0
    0 until s.length foreach { windowEnd =>
      remove(s(windowEnd))

      if ((windowEnd - windowStart + 1) == window) {
        if (map.size == countOfZerosInPatternMap) results.append(windowStart)

        put(s(windowStart))
        windowStart = windowStart + 1
      }
    }

    results.toArray
  }
}
