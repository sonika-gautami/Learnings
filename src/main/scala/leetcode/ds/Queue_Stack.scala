package leetcode.ds

object Queue_Stack extends App {
  {
    assert(canVisitAllRooms(rooms = List(List(1), List(2), List(3), List())))
    assert(canVisitAllRooms(rooms = List(List(2), List(), List(1))))
    assert(!canVisitAllRooms(rooms = List(List(1, 3), List(3, 0, 1), List(2), List(0))))
    assert(canVisitAllRooms(rooms = List(List(1, 3, 2), List(0, 1), List(0), List(0))))
  }

  def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
    import scala.collection.mutable.Stack
    val stack = new Stack[Int]()
    stack.push(0)

    val visited = new Array[Boolean](rooms.length)
    0 until visited.length foreach (i => visited(i) = false)

    while (stack.nonEmpty) {
      val key = stack.pop()

      if (!visited(key)) {
        rooms(key).foreach(stack.push)
        visited(key) = true
      }
    }

    visited.forall(v => v)
  }

  {
    var mat = Array(Array(0, 0, 0), Array(0, 1, 0), Array(0, 0, 0))
    var sol = updateMatrix(mat)
    println(sol.toList.map(_.toList))
    assert(sol.toList.map(_.toList) == List(List(0, 0, 0), List(0, 1, 0), List(0, 0, 0)))

    mat = Array(Array(0, 0, 0), Array(0, 1, 0), Array(1, 1, 1))
    sol = updateMatrix(mat)
    println(sol.toList.map(_.toList))
    assert(sol.toList.map(_.toList) == List(List(0, 0, 0), List(0, 1, 0), List(1, 2, 1)))

    mat = Array(Array(0))
    sol = updateMatrix(mat)
    println(sol.toList.map(_.toList))
    assert(sol.toList.map(_.toList) == List(List(0)))
  }

  def updateMatrix(mat: Array[Array[Int]]): Array[Array[Int]] = {
    import scala.collection.mutable.Queue
    val queue = new Queue[(Int, Int)]()

    def inBound(i: Int, j: Int): Boolean =
      i >= 0 && j >= 0 && i < mat.length && j < mat(i).length

    val sol = new Array[Array[Int]](mat.length)
    0 until (mat.length) foreach { i =>
      sol(i) = new Array[Int](mat(i).length)
      0 until (mat(i).length) foreach { j =>
        if (mat(i)(j) == 0) {
          sol(i)(j) = 0
          queue.enqueue(i -> j)
        } else {
          sol(i)(j) = Int.MaxValue
        }
      }
    }

    def bfsByDist() = {
      while (queue.nonEmpty) {
        val (ci, cj) = queue.dequeue

        List((ci - 1, cj), (ci + 1, cj), (ci, cj - 1), (ci, cj + 1))
          .filter(p => inBound(p._1, p._2))
          .foreach { case (cni, cnj) =>
            if (sol(cni)(cnj) > sol(ci)(cj) + 1) {
              sol(cni)(cnj) = sol(ci)(cj) + 1
              queue.enqueue(cni -> cnj)
            }
          }
      }
    }

    bfsByDist()
    sol
  }

  def updateMatrix_MemoryExceeds2(mat: Array[Array[Int]]): Array[Array[Int]] = {
    val sol = new Array[Array[Int]](mat.length)
    0 until (mat.length) foreach (i => sol(i) = Array.fill(mat(i).length)(-1))

    def inBound(i: Int, j: Int): Boolean =
      i >= 0 && j >= 0 && i < mat.length && j < mat(i).length

    def bfs(i: Int, j: Int): Int = {
      import scala.collection.mutable.Queue
      val queue = new Queue[(Int, Int)]()
      queue.enqueue((i, j))
      var level = 0

      while (queue.nonEmpty) {
        queue.dequeueAll(_ => true).foreach { case (ci, cj) =>
          if (mat(ci)(cj) == 0) return level

          List((ci - 1, cj), (ci + 1, cj), (ci, cj - 1), (ci, cj + 1))
            .filter(p => inBound(p._1, p._2))
            .foreach { case (cni, cnj) =>
              if (mat(cni)(cnj) == 0) return level + 1
              queue.enqueue((cni, cnj))
            }
        }
        level = level + 1
      }
      -1
    }

    0 until (mat.length) foreach { i =>
      0 until (mat(i).length) foreach { j =>
        sol(i)(j) = bfs(i, j)
      }
    }
    sol
  }

  def updateMatrix_MemoryExceeds(mat: Array[Array[Int]]): Array[Array[Int]] = {
    val sol = new Array[Array[Int]](mat.length)
    0 until (mat.length) foreach (i => sol(i) = Array.fill(mat(i).length)(-1))

    def inBound(i: Int, j: Int): Boolean =
      i >= 0 && j >= 0 && i < mat.length && j < mat(i).length

    def bfs(i: Int, j: Int): Int = {
      import scala.collection.mutable.Queue
      val queue = new Queue[(Int, Int)]()
      queue.enqueue((i, j))
      var level = 0

      while (queue.nonEmpty) {
        queue.dequeueAll(_ => true).foreach { case (ci, cj) =>
          if (inBound(ci, cj) && mat(ci)(cj) == 0) return level
          queue.enqueue((ci - 1, cj), (ci + 1, cj), (ci, cj - 1), (ci, cj + 1))
        }
        level = level + 1
      }
      -1
    }

    0 until (mat.length) foreach { i =>
      0 until (mat(i).length) foreach { j =>
        sol(i)(j) = bfs(i, j)
      }
    }
    sol
  }

  {
    var sol = floodFill(image = Array(Array(1, 1, 1), Array(1, 1, 0), Array(1, 0, 1)),
      sr = 1, sc = 1,
      newColor = 2)
    println(sol.toList.map(_.toList))
    assert(sol.toList.map(_.toList) == List(List(2, 2, 2), List(2, 2, 0), List(2, 0, 1)))

    sol = floodFill(image = Array(Array(0, 0, 0), Array(0, 0, 0)),
      sr = 0, sc = 0,
      newColor = 2)
    println(sol.toList.map(_.toList))
    assert(sol.toList.map(_.toList) == List(List(2, 2, 2), List(2, 2, 2)))

    sol = floodFill(image = Array(Array()), sr = 0, sc = 0, newColor = 2)
    println(sol.toList.map(_.toList))
    assert(sol.toList.map(_.toList) == List(List()))

    sol = floodFill(image = Array(), sr = 0, sc = 0, newColor = 2)
    println(sol.toList.map(_.toList))
    assert(sol.isEmpty)
  }

  def floodFill(image: Array[Array[Int]],
                sr: Int, sc: Int,
                newColor: Int): Array[Array[Int]] = {

    def neighbour(i: Int, j: Int): Unit = {
      def inValid =
        i < 0 || j < 0 ||
          i >= image.length || j >= image(i).length ||
          image(i)(j) != image(sr)(sc) ||
          image(i)(j) == newColor ||
          (i == sr && j == sc)

      if (!inValid) {
        image(i)(j) = newColor
        loop(i, j)
      }
    }

    def loop(i: Int, j: Int): Unit = {
      neighbour(i - 1, j)
      neighbour(i + 1, j)
      neighbour(i, j + 1)
      neighbour(i, j - 1)
    }

    if (image == null || image.isEmpty || sr >= image.length) image
    else if (image(sr) == null || image(sr).isEmpty || sc >= image.length) image
    else {
      loop(sr, sc)
      image(sr)(sc) = newColor
      image
    }
  }

  {
    var s = "3[a]2[bc]"
    var sol = decodeString(s)
    println(sol)
    assert(sol == "aaabcbc")

    s = "3[a2[c]]"
    sol = decodeString(s)
    println(sol)
    assert(sol == "accaccacc")

    s = "2[abc]3[cd]ef"
    sol = decodeString(s)
    println(sol)
    assert(sol == "abcabccdcdcdef")

    s = "abc3[cd]xyz"
    sol = decodeString(s)
    println(sol)
    assert(sol == "abccdcdcdxyz")

    s = "100[xyz]"
    sol = decodeString(s)
    println(sol)
  }

  def decodeString(s: String): String = {
    import scala.collection.mutable.Stack
    val stack = new Stack[Char]()

    def decode = {
      var str = ""
      while (stack.top != '[') {
        str = str.+:(stack.pop)
      }
      stack.pop // '['
      var num = stack.pop.asDigit
      var digitPos = 1
      while (stack.nonEmpty && stack.top.isDigit) {
        num = (Math.pow(10, digitPos).toInt * stack.pop.asDigit) + num
        digitPos = digitPos + 1
      }

      (0 until num).foreach { _ =>
        str.foreach(c => stack.push(c))
      }

    }

    s.foreach {
      case ']' => decode
      case c => stack.push(c)
    }

    val sol = new StringBuilder
    while (stack.nonEmpty) {
      sol.append(stack.pop())
    }
    sol.reverse.toString
  }

  val myQueue = new MyQueueWithStack
  println(myQueue.push(1)) // queue is: [1]
  println(myQueue.push(2)) // queue is: [1, 2] (leftmost is front of the queue)
  println(myQueue.peek) // return 1
  println(myQueue.pop) // return 1, queue is [2]
  println(myQueue.empty) // return false

  println("-------------")
  var myStack = new MyStackWithQueue
  println(myStack.push(1))
  println(myStack.push(2))
  println(myStack.top) // return 2
  println(myStack.pop)
  println(myStack.empty) // return False

  myStack = new MyStackWithQueue
  println("-------------")
  println(myStack.push(1))
  println(myStack.push(2))
  println(myStack.push(3))
  println("-------------")
  println(myStack.top) // return 3
  println(myStack.pop) //3
  println(myStack.top) // return 2
  println(myStack.pop) //2
  println(myStack.top) // 1
  println(myStack.empty) // return False
  println(myStack.pop) //1
  println(myStack.empty) // return true
}

class MyStackWithQueue() {

  /** Initialize your data structure here. */
  //with queue ops of push to back, peek/pop from front, size and is empty

  import scala.collection.mutable

  private val queue = mutable.Queue[Int]()
  private val queue2 = mutable.Queue[Int]()

  /** Push element x onto stack. */
  def push(x: Int) {
    queue.enqueue(x)
  }

  /** Removes the element on top of the stack and returns that element. */
  def pop(): Int = {
    if (empty) return -1

    if (queue.nonEmpty) {
      while (queue.size > 1) {
        queue2.enqueue(queue.dequeue())
      }
      queue.dequeue()
    } else {
      while (queue2.size > 1) {
        queue.enqueue(queue2.dequeue())
      }
      queue2.dequeue()
    }
  }

  /** Get the top element. */
  def top(): Int = {
    if (empty) return -1

    if (queue.nonEmpty) {
      while (queue.size > 1) {
        queue2.enqueue(queue.dequeue())
      }
      val t = queue.dequeue()
      queue2.enqueue(t)
      t
    } else {
      while (queue2.size > 1) {
        queue.enqueue(queue2.dequeue())
      }
      val t = queue2.dequeue()
      queue.enqueue(t)
      t
    }
  }

  /** Returns whether the stack is empty. */
  def empty(): Boolean = {
    queue.isEmpty && queue2.isEmpty
  }

}


class MyQueueWithStack {

  /** Initialize your data structure here. */
  //Using Stack witt push to top, peek/pop from top, size, and is empty operations

  import scala.collection.mutable

  private val stack = new mutable.Stack[Int]
  private val stackPop = new mutable.Stack[Int]


  /** Push element x to the back of queue. */
  def push(x: Int) {
    stack.push(x)
  }

  /** Removes the element from in front of queue and returns that element. */
  def pop(): Int = {
    if (this.empty()) -1
    else {
      while (stack.size > 1) {
        stackPop.push(stack.pop())
      }
      val ele = stack.pop()
      while (stackPop.nonEmpty) {
        stack.push(stackPop.pop())
      }
      ele
    }
  }

  /** Get the front element. */
  def peek(): Int = {
    if (this.empty()) -1
    else {
      while (stack.size > 1) {
        stackPop.push(stack.pop())
      }
      val ele = stack.top
      while (stackPop.nonEmpty) {
        stack.push(stackPop.pop())
      }
      ele
    }
  }

  /** Returns whether the queue is empty. */
  def empty(): Boolean = {
    stack.isEmpty
  }

}
