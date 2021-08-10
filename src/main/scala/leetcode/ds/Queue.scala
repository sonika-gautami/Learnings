package leetcode.ds

import scala.collection.mutable


object Queue extends App {

  println("PerfectSquare: " + numSquares(12))
  println("PerfectSquare: " + numSquares(13))
  println("PerfectSquare: " + numSquares(1))
  println("PerfectSquare: " + numSquares(48))
  println("PerfectSquare: " + numSquares(49 * 2))
  println("PerfectSquare: " + numSquares(59))
  println("PerfectSquare: " + numSquares(137))
  println("PerfectSquare: " + numSquares(136))

  //least no of Squares whose sum = number itself
  def numSquares(n: Int): Int = {
    val map = new mutable.HashMap[Int, Int]()

    def loop(num: Int, initialNum: Int = -1): Int = {
      def initialVal: Int = if (initialNum == -1) num / 2 else initialNum

      if (num <= 0) return 0
      if (num == 1) return 1
      if (initialNum == 1) return num
      if (map.contains(num)) return map(num)

      var sqNum = initialVal
      while (sqNum > 0) {
        val sq = sqNum * sqNum
        if (num >= sq) {
          val sol = 1 + loop(num - sq)
          val finalSol = if (sqNum > 1) Math.min(sol, loop(num, sqNum - 1)) else sol
          map.put(num, finalSol)
          return finalSol
        } else {
          sqNum = sqNum - 1
        }
      }
      0
    }

    loop(n)

  }


  {
    val deadends = Array("0201", "0101", "0102", "1212", "2002")
    val target = "0202"
    println("lock =" + openLock(deadends, target))
  }
  {
    val deadends = Array("8888")
    val target = "0009"
    println("lock =" + openLock(deadends, target))
  }
  {
    val deadends = Array("8887", "8889", "8878", "8898", "8788", "8988", "7888", "9888")
    val target = "8888"
    println("lock =" + openLock(deadends, target))
  }
  {
    val deadends = Array("0000")
    val target = "8888"
    println("lock =" + openLock(deadends, target))
    println("lock =" + openLock(deadends, "0000"))
  }

  //Graph BFS
  def openLock(deadends: Array[String], target: String): Int = {

    def nextInGraph(s: String): List[String] = {
      if (s == "9999") Nil
      else
        (0 until s.length).flatMap(i => {
          val incremented = s.slice(0, i) + (10 + s(i).asDigit + 1) % 10 + s.substring(i + 1)
          val decremented = s.slice(0, i) + (10 + s(i).asDigit - 1) % 10 + s.substring(i + 1)
          List(incremented, decremented)
        }).toList
    }

    def loop(initialPos: String): Int = {
      var count = 0
      var queue = new mutable.HashSet[String]
      val lastVisited = new mutable.HashSet[String]
      queue.add(initialPos)

      while (queue.nonEmpty) {
        count = count + 1

        val t = new mutable.HashSet[String]
        queue.diff(lastVisited).foreach { current =>
          val l = nextInGraph(current)
            .filter(s => !lastVisited.contains(s))
            .filter(s => !deadends.contains(s))
          l.find(s => s == target).foreach(_ => return count)

          lastVisited.add(current)
          l.foreach(t.add)
        }
        queue.clear
        queue = t
      }
      -1
    }

    if (target.equals("0000")) 0
    else if (deadends.contains("0000")) -1
    else loop("0000")
  }

  /*def openLock(deadends: Array[String], target: String): Int = {

    def ifStuck(tempT: String) = {
      deadends.contains(tempT)
    }

    def check(i: Int, pre: String, post: String, min: Boolean): Int = {
      val inc = target(i).asDigit - 0
      val dec = 9 - target(i).asDigit + 1
      println(inc + " " + dec)

      if ((min && inc < dec) || (!min && !(inc < dec))) {
        (1 to target(i).asDigit).find(i => ifStuck(pre + i + post))
          .map(stuck => {
            println("Stuck at: " + pre + stuck + post)
            -1
          })
          .getOrElse(inc)
      } else {
        (0 to (9 - target(i).asDigit)).find(i => ifStuck(pre + (9 - i) + post))
          .map(stuck => {
            println("Stuck at: " + pre + stuck + post)
            -1
          })
          .getOrElse(dec)
      }
    }

    def loop(i: Int = 0, initial: String): Int = {
      println(s"loop: $i - $initial")
      if (i >= initial.length) 0
      else {
        var t = check(i, initial.slice(0, i), initial.drop(i + 1), true)
        println(t)
        if (t == -1) {
          t = check(i, initial.slice(0, i), initial.drop(i + 1), false)
          println(t)
        }
        if (t == -1) return -1

        t = t + loop(i + 1, initial.slice(0, i) + target(i) + initial.drop(i + 1))
        t
      }
    }

    loop(0, "0000")
  }*/


  val grid = Array(
    Array('1', '1', '1', '1', '0'),
    Array('1', '1', '0', '1', '0'),
    Array('1', '1', '0', '0', '0'),
    Array('0', '0', '0', '0', '0'))

  val grid2 = Array(
    Array('1', '1', '0', '0', '0'),
    Array('1', '1', '0', '0', '0'),
    Array('0', '0', '1', '0', '0'),
    Array('0', '0', '0', '1', '1'))

  println("numIslands:" + numIslands(grid))
  println("numIslands:" + numIslands(grid2))

  //1 = land, 0 = water, Edges = water|0, Island = surrounded by water
  def numIslands(grid: Array[Array[Char]]): Int = {

    /*(-1 to 1).map(k => tempI + k)
      .filter(k => k >= 0 && k < grid.size)
      .foreach(k => {
        (-1 to 1).map(k2 => tempJ + k2)
          .filter(k2 => k2 >= 0 && k2 < grid(k).size && (k != 0 || k2 != 0))
          .filter(k2 => grid(k)(k2) == '1' && !visited(k)(k2))
          .foreach(k2 => {
            visited(k)(k2) = true
            markVisitedForLand(k, k2)
          })
      })*/

    val visited = new Array[Array[Boolean]](grid.size);
    {
      var i = 0
      while (i < grid.size) {
        visited(i) = new Array[Boolean](grid(i).size)
        i = i + 1
      }
      i = 0
    }

    def markVisitedForLand(tempI: Int, tempJ: Int): Unit = {
      List(-1, 1)
        .map(k => tempI + k)
        .filter(k => k >= 0 && k < grid.size)
        .filter(k => grid(k)(tempJ) == '1' && !visited(k)(tempJ))
        .foreach(k => {
          visited(k)(tempJ) = true
          markVisitedForLand(k, tempJ)
        })

      List(-1, 1)
        .map(k => tempJ + k)
        .filter(k => k >= 0 && k < grid(tempI).size)
        .filter(k => grid(tempI)(k) == '1' && !visited(tempI)(k))
        .foreach(k => {
          visited(tempI)(k) = true
          markVisitedForLand(tempI, k)
        })
    }

    var count = 0
    var j = 0
    var i = 0
    while (i < grid.length) {
      while (j < grid(i).length) {
        if (grid(i)(j) == '1' && !visited(i)(j)) {
          count = count + 1
          visited(i)(j) = true
          markVisitedForLand(i, j)
        }
        j = j + 1
      }
      i = i + 1
      j = 0
    }
    count
  }


  var myCircularQueue = new MyCircularQueue(4)

  println(myCircularQueue.enQueue(3))
  println(myCircularQueue.Front())
  println(myCircularQueue.isFull)
  println(myCircularQueue.enQueue(7))
  println(myCircularQueue.enQueue(2))
  println(myCircularQueue.enQueue(5))
  println(myCircularQueue.deQueue)
  println(myCircularQueue.enQueue(4))
  println(myCircularQueue.enQueue(2))
  println(myCircularQueue.isEmpty())
  println(myCircularQueue.Rear)

  myCircularQueue = new MyCircularQueue(1)
  println(myCircularQueue.enQueue(1))
  println(myCircularQueue.enQueue(2))
}


class MyCircularQueue(_k: Int) {
  private lazy val buffer = new Array[Int](_k)

  private var head: Int = -1
  private var tail: Int = -1

  private def size: Int = {
    if (head == -1) -1
    else if (head <= tail) (tail - head) + 1
    else ((tail + _k) - head) + 1
  }


  def enQueue(value: Int): Boolean = {
    if (isFull()) false
    else {
      if (head == -1) head = 0
      tail = if (tail + 1 >= _k) 0 else (tail + 1)
      buffer(tail) = value
      true
    }
  }

  def deQueue(): Boolean = {
    if (isEmpty()) false
    else {
      if (head == tail) {
        head = -1
        tail = -1
      }
      else head = if (head + 1 >= _k) 0 else (head + 1)
      true
    }
  }

  def Front(): Int = {
    if (isEmpty()) -1
    else buffer(head)
  }

  def Rear(): Int = {
    if (isEmpty()) -1
    else buffer(tail)
  }

  def isEmpty(): Boolean = {
    head == -1
  }

  def isFull(): Boolean = {
    size == _k
  }
}

/**
 * Your MyCircularQueue object will be instantiated and called as such:
 * var obj = new MyCircularQueue(k)
 * var param_1 = obj.enQueue(value)
 * var param_2 = obj.deQueue()
 * var param_3 = obj.Front()
 * var param_4 = obj.Rear()
 * var param_5 = obj.isEmpty()
 * var param_6 = obj.isFull()
 */