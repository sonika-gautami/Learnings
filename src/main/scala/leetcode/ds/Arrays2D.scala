package leetcode.ds

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer

object Arrays2D extends App {

  val a = Array(
    Array(1, 2, 3),
    Array(4, 5, 6),
    Array(7, 8, 9)
  )

  /*
     1 2 3
     4 5 6
     7 8 9
     1 2 4 7 5 3 6 8 9

     1  2  3  4
     5  6  7  8
     9  0  11 12
     13 14 15 16

     1 25 369 47013 81114 1215 16
     */
  def diagonalPath() = {

    def diagonals: ListBuffer[ListBuffer[Int]] = {
      val lenghOf1D = a(0).length - 1
      var i = 0
      var j = 0
      var list = new ListBuffer[ListBuffer[Int]]()

      while (j <= lenghOf1D) {

        val l = new ListBuffer[Int]()
        do {
          l += a(i)(j)
          print(a(i)(j) + " ")

          j = j - 1
          i = i + 1
        } while (j >= 0)

        print(" ")
        list += l
        j = i
        i = 0
      }

      i = 1
      j = lenghOf1D

      while (i <= lenghOf1D) {
        val l = new scala.collection.mutable.ListBuffer[Int]()
        do {
          l += a(i)(j)
          print(a(i)(j) + " ")
          j = j - 1
          i = i + 1
        } while (i <= lenghOf1D)

        print(" ")
        list += l
        i = j + 1 + 1
        j = lenghOf1D
      }

      list
    }

    def reverseOdds(l: ListBuffer[ListBuffer[Int]]) = {
      for {i <- 0 until l.length} yield {
        if (i % 2 != 0) l(i) else l(i).reverse
      }
    }

    val o1 = diagonals
    println()
    println(s"o1: $o1")
    println(reverseOdds(o1))
  }

  diagonalPath()

}
