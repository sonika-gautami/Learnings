package Twenty22

import scala.collection.mutable

object ch6 extends App {
  /*todo -
     html tags
     Solving the Josephus Problem Using a Queue
     execrcises
   */

  var b = htmlTagsMatched(
    """
      |<html>
      |<body>
      |
      |<p>This is a  <br/>  paragraph with    a line break.</p>
      |
      |<h1></h1>
      |</body>
      |</html>
      |
      |
      |""".stripMargin)
  println(b)

  def htmlTagsMatched(html: String): Boolean = {
    val stack = new mutable.ArrayStack[String]();

    var word = ""
    var i = 0
    var tagStart = false

    while (i < html.size) {
      if (html(i) == '<') {
        tagStart = true
        word = word + html(i)
      } else if (html(i) == '>') {
        word = word + html(i)

        if (!word.endsWith("/>")) {
          if (!word.startsWith("</")) stack.push(word)
          else {
            val sTag = stack.pop()
            if (sTag != (word.replace("</", "<"))) return false;
          }
        }

        word = ""
        tagStart = false
      } else if (tagStart) {
        word = word + html(i)
      }
      i = i + 1
    }

    stack.isEmpty
  }

  println(josephusProblem(
    List("Alice", "Bob", "Cindy", "Doug", "Ed", "Fred", "Gh"), 3
  ))
  println("---------------")
  println(josephusProblem_nk_time(
    List("Alice", "Bob", "Cindy", "Doug", "Ed", "Fred", "Gh"), 3
  ))
  /*
  3 ->
  r1 Cindy 2  List("Alice", "Bob", "Cindy", "Doug", "Ed", "Fred", "Gh")  7+2+0 % 7 = 2
  r2 Fred 4   List("Alice", "Bob", "Doug", "Ed", "Fred", "Gh")  6+2+2 % 6 = 4
  r3 Bob 1    List("Alice", "Bob", "Doug", "Ed", "Gh") 5+2+4 % 5 = 1
  r4 Gh 3     List("Alice", "Doug", "Ed", "Gh") 4+2+1 % 4 = 3
  r5 Ed 2     List("Alice", "Doug", "Ed") 3+2+3 % 3 = 2
  r6 Alice 0  List("Alice", "Doug")
  Winner      List("Doug")
   */

  def josephusProblem(elements: List[String], k: Int): String = {
    // o(n^2)
    if (elements.size < 1) return ""

    val outIndex = k - 1
    val ds = scala.collection.mutable.ArrayBuffer(elements: _*)
    println(ds)

    var round = 0
    var lastStart = 0
    while (ds.size > 1) { //n-1 times
      var t = lastStart
      lastStart = (outIndex + ds.size + lastStart) % ds.size
      print(s"${ds.size} $k $t = $lastStart")

      println(s"Round No - ${round + 1}; Out = ${ds(lastStart)} ${ds}")
      ds.remove(lastStart) //it's own time is o(n)
      round = round + 1
    }

    println(s"Winner = ${ds(0)}");
    ds(0)
  }

  def josephusProblem_nk_time(elements: List[String], k: Int): String = {
    val q = CircularQueue(elements)

    var lastEle: String = ""
    while (!q.isEmpty) {
      (0 until (k - 1)) foreach (_ => q.rotate())
      lastEle = q.dequeue()
      if (q.isEmpty) {
        println(s"Winner is $lastEle")
        return lastEle
      } else {
        println(s"Out is $lastEle")
      }
    }
    lastEle
  }
}


class CircularQueue[T] {

  import scala.collection.mutable.ArrayBuffer

  var arr: ArrayBuffer[T] = _
  var head = -1
  var tail = -1

  def capacity(s: Int) = new ArrayBuffer[T](s)

  def addAll(eles: List[T]) = {
    arr = eles.to[ArrayBuffer]
    head = 0
    tail = eles.size - 1
    println(arr.toList)
  }

  def enqueue(obj: T) = {
    if (isFull) throw new Exception("Queue is full")
    val newTail = (tail + 1 + arr.size) % arr.size
    arr(newTail) = obj
    tail = newTail
  }

  def dequeue(): T = {
    if (isEmpty) throw new Exception("Queue is empty")
    val e = arr(head)
    head = (head + 1 + arr.size) % arr.size
    e
  }

  def rotate() = enqueue(dequeue())

  def isFull: Boolean = (tail + 1 + arr.size) % arr.size == head

  def isEmpty: Boolean = tail == -1
}

object CircularQueue {
  def apply[T](elements: List[T]) = {
    val q = new CircularQueue[T]()
    q.addAll(elements)
    q
  }

  def apply[T](capacity: Int) = {
    val q = new CircularQueue[T]()
    q.capacity(capacity)
    q
  }
}