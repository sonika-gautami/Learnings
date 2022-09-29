package grokking._2

import grokking.Utils

object _15 extends App {
  Utils.print(() =>
    findAllWordConcat("catfoxcat", Array("cat", "fox")).toList,
    List(0, 3)
  )
  Utils.print(() =>
    findAllWordConcat("catcatfoxfox", Array("cat", "fox")).toList,
    List(3)
  )

  def findAllWordConcat(s: String, words: Array[String]): Array[Int] = {
    val wordsMap = words.groupBy(s => s).mapValues(_.length)

    import scala.collection.mutable.ArrayBuffer
    val resultIndex = new ArrayBuffer[Int]()

    val wordLength = words.head.length
    val window = words.size * wordLength

    0 to (s.length - window) foreach { start =>
      import scala.collection.mutable
      val map = new mutable.HashMap[String, Int]()

      var end = start
      while (end != Int.MaxValue && (end - start + 1) <= window) {
        val word = s.substring(end, end + wordLength)

        if (!wordsMap.contains(word)) end = Int.MaxValue //to exit
        //to prevent for both map compare - check the freq is >
        else if (map.contains(word) && wordsMap(word) == map(word)) end = Int.MaxValue //to exit
        else {
          map.put(word, map.getOrElse(word, 0) + 1)
          end = end + wordLength
        }
      }
      if (end != Int.MaxValue) resultIndex.append(start)
    }

    resultIndex.toArray
  }

  //Given all words are of the same length.
  def findAllWordConcat_Try2(str: String, words: Array[String]): Array[Int] = {
    val wordsMap = words.groupBy(s => s).mapValues(_.length)

    import scala.collection.mutable
    val map = new mutable.HashMap[String, Int]()

    import scala.collection.mutable.ArrayBuffer
    val resultIndex = new ArrayBuffer[Int]()

    var countOfZeros = 0

    def add(s: String): Unit = if (map.contains(s)) {
      if (map(s) == 0) countOfZeros = countOfZeros - 1
      map.put(s, map(s) + 1)
    }

    def remove(s: String): Unit = if (map.contains(s)) {
      map.put(s, map(s) - 1)
      if (map(s) == 0) countOfZeros = countOfZeros + 1
    }

    val wordLength = words.head.length
    val window = words.size * wordLength
    var windowStart = 0

    var windowEnd = 0
    while (windowEnd <= (str.length - wordLength)) {
      println(map)
      remove(str.substring(windowEnd, windowEnd + wordLength))
      println(windowEnd + "- " + windowStart + "-" + str.substring(windowEnd, windowEnd + wordLength))
      println(map)

      if (window == (windowEnd - windowStart + 1)) {
        if (countOfZeros == map.size) resultIndex.append(windowStart)

        add(str.substring(windowStart, windowStart + wordLength))
        windowStart = windowStart + 1
      }
      windowEnd = windowEnd + 1
    }

    resultIndex.toArray
  }

  def findAllWordConcat_WrongTry(str: String, words: Array[String]): Array[Int] = {
    import scala.collection.mutable
    val map = new mutable.HashMap[String, Int]()
    map.++=(words.groupBy(s => s).mapValues(_.length))

    import scala.collection.mutable.ArrayBuffer
    val resultIndex = new ArrayBuffer[Int]()

    var countOfZeros = 0
    var windowStartStr = ""

    def add: Unit = if (map.contains(windowStartStr)) {
      if (map(windowStartStr) == 0) countOfZeros = countOfZeros + 1
      map.put(windowStartStr, map(windowStartStr) + 1)
    }

    def remove(s: String): Unit = if (map.contains(s)) {
      map.put(s, map(s) - 1)
      if (map(s) == 0) countOfZeros = countOfZeros - 1
    }

    val wordLength = words.head.length
    val window = words.size * wordLength
    var windowStart = 0
    val sb = new StringBuilder

    0 until str.length foreach { windowEnd =>
      sb.append(str(windowEnd) + "-" + windowStartStr)
      println(sb)
      if ((windowEnd - windowStart + 1) % wordLength == 0) {
        remove(sb.toString())
        if (windowEnd == windowStart + wordLength) windowStartStr = sb.toString()
        sb.clear()
      }

      if (windowEnd - windowStart + 1 == window) {
        if (countOfZeros == map.size) resultIndex.append(windowStart)
        add
        windowStartStr = ""
        windowStart = windowStart + 1
      }
    }

    resultIndex.toArray
  }
}
