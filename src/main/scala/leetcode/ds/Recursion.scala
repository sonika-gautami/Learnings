package leetcode.ds


class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x

  override def toString: String = s"x= $x next= $next"
}

class TreeNodeR(_value: Int = 0, _left: TreeNodeR = null, _right: TreeNodeR = null) {
  var value: Int = _value
  var left: TreeNodeR = _left
  var right: TreeNodeR = _right

  override def toString: String = s"[v: $value l: $left r: $right]"
}

object Recursion extends App {

  {
    val sol: Int = kthGrammar(n = 1, k = 1)
    println(sol)
    assert(sol == 0)
    assert(kthGrammar(n = 2, k = 1) == 0)
    assert(kthGrammar(n = 2, k = 2) == 1)
    assert(kthGrammar(n = 3, k = 1) == 0)
  }

  def kthGrammar(n: Int, k: Int): Int = {
    @scala.annotation.tailrec
    def loop(i: Int, acc: String): String = {
      if (i >= (n - 1)) acc
      else loop(i + 1, acc.flatMap(c => if (c == '0') "01" else "10"))
    }

    loop(0, "0")(k - 1).asDigit
  }

  def kthGrammar_MemoryExceeds(n: Int, k: Int): Int = {
    @scala.annotation.tailrec
    def loop(i: Int, acc: String): String = {
      if (i >= (n - 1)) acc
      else loop(i + 1, acc.flatMap(c => if (c == '0') "01" else "10"))
    }

    loop(0, "0")(k - 1).asDigit
  }


  {
    var sol = mergeTwoLists(
      l1 = new ListNode(1, new ListNode(2, new ListNode(4))),
      l2 = new ListNode(1, new ListNode(3, new ListNode(4))))
    println(sol)

    sol = mergeTwoLists(l1 = null, l2 = null)
    println(sol)

    sol = mergeTwoLists(l1 = null, l2 = new ListNode(0))
    println(sol)
  }

  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {

    @scala.annotation.tailrec
    def loop(t1: ListNode, t2: ListNode, acc: ListNode): Unit = {
      if (t1 == null) acc.next = t2
      else if (t2 == null) acc.next = t1
      else if (t1.x == t2.x) {
        val t1Temp = t1.next
        val t2Temp = t2.next
        acc.next = t1
        t1.next = t2
        loop(t1Temp, t2Temp, t2)
      } else if (t1.x < t2.x) {
        val temp = t1.next
        acc.next = t1
        loop(temp, t2, t1)
      } else {
        val temp = t2.next
        acc.next = t2
        loop(t1, temp, t2)
      }
    }

    val node = new ListNode()
    loop(l1, l2, node)
    node.next
  }


  {
    val t = new TreeNodeR(4,
      new TreeNodeR(2,
        new TreeNodeR(1),
        new TreeNodeR(3)),
      new TreeNodeR(7))
    println(maxDepth(t))
  }

  def maxDepth(root: TreeNodeR): Int = {
    var max = 0

    def loop(t: TreeNodeR, level: Int): Unit = {
      println(level + "::" + t)
      if (t == null) {
        if (level > max) max = level
      }
      else {
        loop(t.left, level + 1)
        loop(t.right, level + 1)
      }
    }

    loop(root, 0)
    max
  }


  println(myPow(2, 10))
  println(myPow(2.1, 3))
  println(myPow(2, -2))

  println(myPow(0.00001, 2147483647))

  def myPow(x: Double, n: Int): Double = {

    @scala.annotation.tailrec
    def loop(ith: Int, acc: Double): Double = {
      if (ith == 0) acc
      else if (acc == 0.0) 0.0
      else if (ith < 0) loop(ith + 1, acc / x)
      else loop(ith - 1, acc * x)
    }

    if (x == 1.0) 1.0
    else if (x == -1.0) if (n % 2 == 0) 1.0 else -1.0
    else loop(n, 1)
  }

  def myPow_StackOverFlow(x: Double, n: Int): Double = {

    import scala.collection.mutable.HashMap

    val map = new HashMap[Int, Double]()

    def loop(ith: Int): Double = {
      println(ith + " :: " + map)
      if (ith == 1) x
      else if (ith == 0) 1
      else if (map.contains(ith)) map(ith)
      else if (ith < 0) {
        val t = loop(ith + 1) / x
        map.put(ith, t)
        t
      }
      else {
        val t = x * loop(ith - 1)
        map.put(ith, t)
        t
      }
    }

    loop(n)
  }

  var sol = climbStairs(2)
  println(sol)
  assert(sol == 2)

  sol = climbStairs(3)
  println(sol)
  assert(sol == 3)

  sol = climbStairs(2)
  println(sol)
  assert(sol == 2)

  sol = climbStairs(4)
  println(sol)
  assert(sol == 5)

  def climbStairs(n: Int): Int = {
    val cache = if (n < 2) null else new Array[Int](n + 1 - 2)

    def loop(i: Int): Int = {
      if (i == 0) 1
      else if (i == 1) 2
      else if (cache(i - 2) != 0) cache(i - 2)
      else {
        cache(i - 2) = loop(i - 2) + loop(i - 1)
        cache(i - 2)
      }
    }

    loop(n - 1)
  }

  def climbStairs_NA(n: Int): Int = {
    val max2 = n / 2
    val maxHasAllTwos = if (n % 2 == 0) 1 else 0
    var ways = 1 + maxHasAllTwos
    (1 to (max2 - maxHasAllTwos)).foreach {
      twos =>
        val ones = n - (twos * 2)
        ways = ways + (ones + twos)
    }
    ways
  }

  println("-----------")
  println(fib(0))
  println(fib(1))
  println(fib(2))
  println(fib(3))
  println(fib(4))
  println(fib(5))
  println(fib(6))

  def fib(n: Int): Int = {
    val cache = if (n < 2) null else new Array[Int](n + 1 - 2)

    def loop(i: Int): Int = {
      if (i < 2) i
      else if (cache(i - 2) != 0) cache(i - 2)
      else {
        cache(i - 2) = loop(i - 2) + loop(i - 1)
        cache(i - 2)
      }
    }

    loop(n)
  }

  println(getRow(3))
  println(getRow(2))
  println(getRow(1))
  println(getRow(0))

  //Pascal
  def getRow(rowIndex: Int): List[Int] = {
    val cache = new Array[Array[Int]](rowIndex + 1)
    0 to rowIndex foreach (i => cache(i) = new Array[Int](i + 1))

    def loop(i: Int, j: Int): Int = {
      if (cache(i)(j) != 0) cache(i)(j)
      else {
        if (j == 0 || j == i) cache(i)(j) = 1
        else cache(i)(j) = loop(i - 1, j - 1) + loop(i - 1, j)
        cache(i)(j)
      }
    }

    (0 to rowIndex).map(loop(rowIndex, _)).toList
  }
  {
    val t = new TreeNodeR(4,
      new TreeNodeR(2,
        new TreeNodeR(1),
        new TreeNodeR(3)),
      new TreeNodeR(7))

    var sol = searchBST(t, 2)
    println(sol)

    sol = searchBST(t, 5)
    println(sol)
  }

  def searchBST(root: TreeNodeR, `val`: Int): TreeNodeR = {

    def loop(t: TreeNodeR): TreeNodeR = {
      if (t == null) null
      else if (t.value == `val`) t
      else {
        val left = loop(t.left)
        if (left == null) loop(t.right) else left
      }
    }

    loop(root)
  }

  def searchBST_BadTimer(root: TreeNodeR, `val`: Int): TreeNodeR = {

    import scala.collection.mutable.Queue

    val queue = new Queue[TreeNodeR]()
    if (root != null) queue.enqueue(root)

    while (queue.nonEmpty) {
      queue.dequeueAll(_ => true).foreach {
        e =>
          if (e.value == `val`) return e

          if (e.left != null) queue.enqueue(e.left)
          if (e.right != null) queue.enqueue(e.right)
      }
    }
    null
  }
  {
    val n1 = new ListNode(1,
      new ListNode(2,
        new ListNode(3,
          new ListNode(4))))
    var sol = swapPairs(head = n1)
    println(sol) //Output: [2,1,4,3]

    val n2 = new ListNode()
    sol = swapPairs(head = n2)
    println(sol)

    val n3 = new ListNode(1)
    sol = swapPairs(head = n3)
    println(sol)

    val n4 = new ListNode(1,
      new ListNode(2,
        new ListNode(3)))
    sol = swapPairs(head = n4)
    println(sol)

    println("-----Reverse List--------")
    println(reverseList(n1))
    println(reverseList(n2))
    println(reverseList(n3))
    println(reverseList(n4))

    println(reverseListIterative(n1))
    println(reverseListIterative(n2))
    println(reverseListIterative(n3))
    println(reverseListIterative(n4))
  }

  def reverseListModifyExisting(head: ListNode): ListNode = {
    head
  }

  def reverseListIterative(head: ListNode): ListNode = {
    var prev: ListNode = null
    var t = head
    while (t != null) {
      prev = new ListNode(t.x, prev)
      t = t.next
    }
    prev
  }

  def reverseList(head: ListNode): ListNode = {
    @scala.annotation.tailrec
    def loop(h: ListNode, acc: ListNode): ListNode = {
      if (h == null) acc
      else loop(h.next, new ListNode(h.x, acc))
    }

    loop(head, null)
  }

  def reverseList_BadTimer(head: ListNode): ListNode = {
    def loop(h: ListNode): ListNode = {
      if (h == null) null
      else if (h.next == null) new ListNode(h.x)
      else {
        val init = loop(h.next)

        var t = init
        while (t.next != null) t = t.next
        t.next = new ListNode(h.x)

        init
      }
    }

    loop(head)
  }

  def swapPairs(head: ListNode): ListNode = {

    def swap(h: ListNode): ListNode = {
      if (h == null) null
      else if (h.next == null) h
      else {
        new ListNode(h.next.x, new ListNode(h.x, swap(h.next.next)))
      }
    }

    swap(head)
  }


  println(reverseString("hello".toCharArray))
  println(reverseString("hello!".toCharArray))
  println(reverseString("h".toCharArray))
  println(reverseString("hi".toCharArray))

  def reverseString(s: Array[Char]): Unit = {
    def loop(index: Int): Unit = {
      if (index < (s.length / 2)) {
        val t = s(index)
        s(index) = s(s.length - 1 - index)
        s(s.length - 1 - index) = t
        loop(index + 1)
      }
    }

    loop(0)
  }

}
