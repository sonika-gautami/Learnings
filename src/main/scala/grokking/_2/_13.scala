package grokking._2

object _13 extends App {

  Utils.print(() =>
    smallestSubStringContainingPattern("aabdec", "abc"),
    "abdec"
  )
  Utils.print(() =>
    smallestSubStringContainingPattern("abdabca", "abc"),
    "abc"
  )
  Utils.print(() =>
    smallestSubStringContainingPattern("adcad", "abc"),
    ""
  )


  //similar to Permutation/Anagram; only diff is window length is when map has all zeros.
  def smallestSubStringContainingPattern(s: String, t: String): String = {

    import scala.collection.mutable
    val map = new mutable.HashMap[Char, Int]()
    map.++=(t.groupBy(c => c).mapValues(_.length))

    var countOfZerosInPatternMap = 0

    def put(c: Char): Unit = if (map.contains(c)) {
      if (map(c) == 0) countOfZerosInPatternMap = countOfZerosInPatternMap - 1
      map.put(c, map(c) + 1)
    }

    def remove(c: Char): Unit = if (map.contains(c)) {
      map.put(c, map(c) - 1)
      if (map(c) == 0) countOfZerosInPatternMap = countOfZerosInPatternMap + 1
    }

    var windowStart = 0
    var resultStrStartIndex = 0
    var resultStrEndIndex = Int.MaxValue
    0 until s.length foreach { windowEnd =>
      remove(s(windowEnd))

      while (map.size == countOfZerosInPatternMap) {
        if ((resultStrEndIndex - resultStrStartIndex) > (windowEnd - windowStart)) {
          resultStrStartIndex = windowStart
          resultStrEndIndex = windowEnd
        }

        put(s(windowStart))
        windowStart = windowStart + 1
      }
    }

    if (resultStrEndIndex == Int.MaxValue) ""
    else s.substring(resultStrStartIndex, resultStrEndIndex + 1)
  }
}
