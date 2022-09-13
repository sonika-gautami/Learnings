package grokking._2

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object _4 extends App {

  println(longestDistinctChars("araaci", 2))
  println(longestDistinctChars("araaci", 1))
  println(longestDistinctChars("cbbebi", 3))

  def longestDistinctChars(str: String, k: Int): List[String] = {

    var longestSize = -1
    val longestStrs = new ArrayBuffer[String]()

    0 until str.length foreach { i =>

      val distinctChars = new mutable.HashSet[Char]()
      var j = i
      while (j < str.length &&
        (distinctChars.contains(str(j)) || //char part of disctinct
          distinctChars.size < k)) { //new char allowed
        distinctChars.add(str(i))
        j = j + 1
      }
      if ((j - i) > longestSize) {
        longestSize = (j - i)
        longestStrs.clear
        longestStrs.append(str.substring(i, j))
      } else if ((j - i) == longestSize) {
        longestStrs.append(str.substring(i, j))
      }

    }

    println(longestSize)
    longestStrs.toList
  }
}
