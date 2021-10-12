package leetcode.ds

import scala.annotation.tailrec

object Recursion2Part3 extends App {

  {
    var sol: List[List[Int]] = Nil
    var buildings = List(List(2, 9, 10), List(3, 7, 15), List(5, 12, 12), List(15, 20, 10), List(19, 24, 8))
    sol = getSkyline(buildings.map(_.toArray).toArray)
    //assert(sol == List(List(2, 10), List(3, 15), List(7, 12), List(12, 0), List(15, 10), List(20, 8), List(24, 0)))

    buildings = List(List(0, 2, 3), List(2, 5, 3))
    sol = getSkyline(buildings.map(_.toArray).toArray)
    //assert(sol == List(List(0, 3), List(5, 0)))

    def getSkyline(buildings: Array[Array[Int]]): List[List[Int]] = {
      Nil
    }
  }

  {
    var sol: List[String] = Nil
    sol = letterCombinations(digits = "23")
    println(sol)
    assert(sol.forall(s => List("ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf").contains(s)))

    sol = letterCombinations(digits = "")
    assert(sol == Nil)

    sol = letterCombinations(digits = "2")
    assert(sol == List("a", "b", "c"))

    def letterCombinations(digits: String): List[String] = {
        val map = Map(
          '2' -> List('a', 'b', 'c'),
          '3' -> List('d', 'e', 'f'),
          '4' -> List('g', 'h', 'i'),
          '5' -> List('j', 'k', 'l'),
          '6' -> List('m', 'n', 'o'),
          '7' -> List('p', 'q', 'r', 's'),
          '8' -> List('t', 'u', 'v'),
          '9' -> List('w', 'x', 'y', 'z')
        )

        def loop(i: Int, acc: List[String]): List[String] = {
          if (i >= digits.length) acc
          else
            loop(i + 1,
              map(digits(i)).flatMap(c => acc.map(s => s.:+(c))))
        }

        if (digits.isEmpty) Nil else loop(1, map(digits(0)).map(_.toString))
    }
  }

  {
    var sol: Int = 0
    sol = largestRectangleArea(heights = Array(2, 1, 5, 6, 2, 3))
    assert(sol == 10)

    sol = largestRectangleArea(heights = Array(2, 4))
    assert(sol == 4)

    def largestRectangleArea(heights: Array[Int]): Int = {
      var max = -1

      def calculateMax(area: Int) = if (area > max) max = area

      @tailrec
      def loop(level: Int, l: Int, b: Int): Unit = {
        if (level < heights.length) {
          val newL = Math.min(l, heights(level))
          calculateMax(newL * (b + 1))

          loop(level + 1, newL, b + 1)
        }
      }

      (0 until heights.length).foreach(i => {
        calculateMax(heights(i) * 1)
        loop(i + 1, heights(i), 1)
      })
      max
    }
  }

  {

    var sol: List[List[Int]] = Nil
    sol = permute(Array(1, 2, 3))
    println(sol)

    sol = permute(Array(1, 2))
    println(sol)

    sol = permute(Array(1))
    println(sol)

    def permute(nums: Array[Int]): List[List[Int]] = {
      def loop(level: Int, acc: List[List[Int]]): List[List[Int]] = {
        if (level == nums.length) acc
        else loop(level + 1,
          acc.flatMap(l => nums.filter(n => !l.contains(n)).map(n => l :+ n)))
      }

      nums.flatMap(n => loop(1, List(List(n)))).toList
    }
  }

}
