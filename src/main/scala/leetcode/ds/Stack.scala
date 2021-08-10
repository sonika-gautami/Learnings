package leetcode.ds

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Node {
  var value = 0
  var neighbors: List[Node] = Nil

  override def toString: String = s"val: $value; ne: ${neighbors.map(_.value)}"
}

object Node {
  def apply(v: Int, ne: List[Node]): Node = {
    val n = new Node
    n.value = v
    n.neighbors = ne
    n
  }

  def apply(v: Int): Node = {
    val n = new Node
    n.value = v
    n
  }
}

object Stack extends App {

  {
    var sol = findTargetSumWays(nums = Array(1, 1, 1, 1, 1), target = 3)
    println(sol)
    assert(sol == 5)

    sol = findTargetSumWays(nums = Array(1), target = 1)
    println(sol)
    assert(sol == 1)
  }

  //all possible ways with + - to achieve target
  def findTargetSumWays(nums: Array[Int], target: Int): Int = {

  }


  {
    val node1 = Node(1)
    val node2 = Node(2)
    val node3 = Node(3)
    val node4 = Node(4)
    node1.neighbors = List(node2, node4)
    node2.neighbors = List(node1, node3)
    node3.neighbors = List(node2, node4)
    node4.neighbors = List(node1, node3)

    println(node1)
    println(cloneGraph(node1))


    def cloneGraph(graph: Node): Node = {
      if (graph == null) return null

      val map = new mutable.HashMap[Int, Node]()

      def clonedNode(curr: Node, f: () => Unit) = {
        if (map.contains(curr.value)) map(curr.value)
        else {
          val n = Node(curr.value)
          f()
          map.put(curr.value, n)
          n
        }
      }

      val queue = new mutable.Queue[Node]
      queue.enqueue(graph)
      while (queue.nonEmpty) {
        val curr = queue.dequeue()
        clonedNode(curr, () => {}).neighbors =
          curr.neighbors.map(ne => clonedNode(ne, () => queue.enqueue(ne)))
      }

      map(graph.value)
    }
  }


  {
    var tokens = Array("2", "1", "+", "3", "*")
    var solution = evalRPN(tokens)
    println(solution)
    assert(solution == 9)

    tokens = Array("4", "13", "5", "/", "+")
    solution = evalRPN(tokens)
    println(solution)
    assert(solution == 6)

    tokens = Array("10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+")
    solution = evalRPN(tokens)
    println(solution)
    assert(solution == 22)
  }

  def evalRPN(tokens: Array[String]): Int = {
    def isOp(op: String): Boolean =
      op == "+" || op == "-" || op == "/" || op == "*"

    def eval(l: Int, r: Int, op: String): Int =
      op match {
        case "+" => l + r
        case "-" => l - r
        case "/" => l / r
        case "*" => l * r
      }

    val stack = new mutable.Stack[Int]()
    tokens.foreach { t =>
      if (isOp(t)) {
        val r = stack.pop()
        val l = stack.pop()
        stack.push(eval(l, r, t))
      } else {
        stack.push(t.toInt)
      }
    }
    stack.pop()
  }


  {
    var temperatures = Array(1, 3, 5, 9, 7, 8, 2, 4)
    var solution: Array[Int] = dailyTemperatures(temperatures)
    println(solution.toList)

    temperatures = Array(73, 74, 75, 71, 69, 72, 76, 73)
    solution = dailyTemperatures(temperatures)
    println(solution.toList)
    assert(solution.toList == List(1, 1, 4, 2, 1, 1, 0, 0))

    temperatures = Array(30, 40, 50, 60)
    solution = dailyTemperatures(temperatures)
    println(solution.toList)
    assert(solution.toList == List(1, 1, 1, 0))

    temperatures = Array(30, 60, 90)
    solution = dailyTemperatures(temperatures)
    println(solution.toList)
    assert(solution.toList == List(1, 1, 0))

    temperatures = Array(30)
    solution = dailyTemperatures(temperatures)
    println(solution.toList)
    assert(solution.toList == List(0))

    temperatures = Array(89, 62, 70, 58, 47, 47, 46, 76, 100, 70)
    solution = dailyTemperatures(temperatures)
    println(solution.toList)
    assert(solution.toList == List(8, 1, 5, 4, 3, 2, 1, 1, 0, 0))

    temperatures = Array(55, 38, 53, 81, 61, 93, 97, 32, 43, 78)
    solution = dailyTemperatures(temperatures)
    println(solution.toList)
    assert(solution.toList == List(3, 1, 1, 2, 1, 1, 0, 1, 1, 0))

    //    temperatures = ((0 until 200000).map(_ => 30).toList ++ List(31)).toArray
    //    solution = dailyTemperatures(temperatures)
    //    println(solution.toList)
  }

  def dailyTemperatures(temperatures: Array[Int]): Array[Int] = {
    val stack = new mutable.Stack[Int]()
    val solution = new ArrayBuffer[Int](temperatures.length)
    (0 until temperatures.length).foreach(_ => solution.append(0))

    (0 until temperatures.length).foreach { i =>
      while (stack.nonEmpty && temperatures(stack.top) < temperatures(i)) {
        solution(stack.top) = (i - stack.top)
        stack.pop()
      }
      stack.push(i)
    }
    solution.toArray
  }

  def dailyTemperatures_NotWorth(temperatures: Array[Int]): Array[Int] = {
    val stack = new mutable.Queue[Int]()

    var lastAns: Int = 0
    (0 until temperatures.length).map { i =>

      if (i > 0 && temperatures(i) == temperatures(i - 1)) {
        lastAns = lastAns - 1
        lastAns
      } else if (stack.nonEmpty) {
        println(temperatures(i) + " " + stack)
        val count = stack.size
        stack.dequeue
        lastAns = count
        count
      }
      else {
        var last: Int = 0
        var count = 0
        (i + 1 until temperatures.length).find(j => {

          if (j == i + 1) last = temperatures(j)
          else if (temperatures(j) > last) {
            stack.enqueue(temperatures(j))
            last = temperatures(j)
          }
          else last = Int.MaxValue

          count = count + 1
          temperatures(j) > temperatures(i)
        })
          .map(_ => {
            println(count)
            lastAns = count
            count
          }).getOrElse({
          lastAns = 0
          0
        })
      }
    }.toArray
  }

  def dailyTemperatures_TimeExceeds(temperatures: Array[Int]): Array[Int] = {
    (0 until temperatures.length).map {
      i =>

        var count = 0
        (i + 1 until temperatures.length).find(j => {
          count = count + 1
          temperatures(j) > temperatures(i)
        })
          .map(_ => count).getOrElse(0)
    }.toArray
  }
  {
    var s = "()"
    println(isValid(s))

    s = "()[]{}"
    println(isValid(s))

    s = "(]"
    println(isValid(s))

    s = "([)]"
    println(isValid(s))

    s = "{[]}"
    println(isValid(s))
  }


  def isValid(s: String): Boolean = {
    val stack = new mutable.Stack[Char]

    def loop(s: String): Boolean = {
      s.foreach {
        c =>
          val b = c match {
            case ')' => stack.nonEmpty && '(' == stack.pop()
            case '}' => stack.nonEmpty && '{' == stack.pop()
            case ']' => stack.nonEmpty && '[' == stack.pop()
            case _ => {
              stack.push(c)
              true
            }
          }
          if (!b) return false
      }

      stack.isEmpty
    }

    loop(s)
  }


  val minStack = new MinStack
  println(minStack.push(-2))
  println(minStack.push(0))
  println(minStack.push(-3))
  println(minStack.getMin) // return -3

  println(minStack.pop())
  println(minStack.top) // return 0

  println(minStack.getMin) // return -2
  println(minStack.pop())
  println(minStack.pop())
  println(minStack.push(-2))
  println(minStack.getMin) // return -2
  println(minStack.top) // return 0
}

class MinStack() {

  /** initialize your data structure here. */
  var size = 100
  var array = new ArrayBuffer[Int](size)
  var _top: Int = -1
  var min: Int = Int.MaxValue

  private def reSizeArray = {
    size = size * 2
    val temp = array
    array = new ArrayBuffer[Int](size)
    array.++=(temp)
  }

  private def isEmpty = _top == -1

  private def assignMin = {
    min = if (isEmpty) Int.MaxValue else array.min
  }

  def push(`val`: Int) {
    if (_top == size - 1) reSizeArray
    if (min > `val`) min = `val`
    array.append(`val`)
    _top = _top + 1
  }

  def pop() {
    if (min == array(_top)) {
      array.remove(_top)
      _top = _top - 1
      assignMin
    } else {
      array.remove(_top)
      _top = _top - 1
    }
  }

  def top(): Int = {
    array(_top)
  }

  def getMin(): Int = {
    min
  }

}

/**
 * Your MinStack object will be instantiated and called as such:
 * var obj = new MinStack()
 * obj.push(`val`)
 * obj.pop()
 * var param_3 = obj.top()
 * var param_4 = obj.getMin()
 */