package Twenty22

import scala.collection.mutable.Queue

object SummationPuzzle extends App {

  puzzle("pot", "pan", "bib") //one sol -> 103 + 149 = 252 p-1,o-0,t-3,a-4,n-9,b-2,i-5
  puzzle("boy", "girl", "baby")
  puzzle("dog", "cat", "pig")

  def puzzle(s1: String, s2: String, result: String) = {
    val uniques = (s1 + s2 + result).distinct
    val queue = new Queue[Int]()

    def assign(index: Int, valuesIndex: Int) = {
      uniques(index)
    }



    lazy val values = (0 to 9).toList

    def equalCheck(m: Map[Char, Int]): Boolean = {
      val b = (s1 + s2).map(m(_)).sum == result.map(m(_)).sum
      if (b) println(s"Solution For $s1 + $s2 = $result -> $m")
      b
    }
  }

}
