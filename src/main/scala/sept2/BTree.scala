package sept2

object BTree extends App {
  var treeNode = Node(6,
    Node(21,
      Node(1),
      Node(4, Node(-31), Node(5))),
    Node(7, right = Node(9, left = Node(8, left = Node(90, right = Node(91))))))
  println(height(treeNode))

  println(levelTraversal(treeNode))
  //6 21 7 1 4 9 -31 5 8 90 90
  //6 21 7 1 4 9 -31 5 8 90 91

  val node = decode(Array(
    Array(2, 3),
    Array(-1, 4),
    Array(-1, 5),
    Array(-1, -1),
    Array(-1, -1)
  ))
  println(node)
  println(encode(node).map(_.toSeq).toSeq)

  private val inouts = Array(
    Array(2, 3),
    Array(4, -1),
    Array(5, -1),
    Array(6, -1),
    Array(7, 8),
    Array(-1, 9),
    Array(-1, -1),
    Array(10, 11),
    Array(-1, -1),
    Array(-1, -1),
    Array(-1, -1)
  )
  val node2 = decode(inouts)
  println(node2)
  println(encode(node2).map(_.toSeq).toSeq)
  println(encode(swapTree(node2, Array(2, 4))).map(_.toSeq).toSeq)
  println(inOrderTraverse(swapTree(node2, Array(2, 4))).toList)

  val list = for (i <- 0 until Array(2, 4).length)
    yield inOrderTraverse(swapTree(node, Array(2, 4).drop(i)))
  println(list.toArray)

  println(swapNodes(inouts, Array(2, 4)).toList.map(_.toList))

  Node(1,
    Node(2,
      Node(4,
        null,
        Node(7,
          Node(12, null, null),
          Node(13, null, null))
      ),
      Node(5,
        Node(8,
          null,
          Node(14, null, null)
        ),
        Node(9, null, null))
    ),
    Node(3,
      Node(6,
        Node(10,
          Node(15, null, null),
          null),
        Node(11,
          Node(16, null, null),
          Node(17, null, null)
        )
      ),
      null)
  )

  private val inputs2 = Array(
    Array(2, 3),
    Array(4, 5),
    Array(6, -1),
    Array(-1, 7),
    Array(8, 9),
    Array(10, 11),
    Array(12, 13),
    Array(-1, 14),
    Array(-1, -1),
    Array(15, -1),
    Array(16, 17),
    Array(-1, -1),
    Array(-1, -1),
    Array(-1, -1),
    Array(-1, -1),
    Array(-1, -1),
    Array(-1, -1)
  )
  println(swapNodes(inputs2, Array(2, 3)).toList.map(_.toList))
  println(decode(inputs2))
  //    14 8 5 9 2 4 13 7 12 1 3 10 15 6 17 11 16
  //    9 5 14 8 2 13 7 12 4 1 3 17 11 16 6 10 15
  //
  //  9 5 8 14 2 12 7 13 4 1 3 16 11 17 6 15 10
  //  8 14 5 9 2 4 12 7 13 1 3 15 10 6 16 11 17


  def height(t: Node): Int = {
    def loop(r: Node, acc: Int): Int = {
      if (r == null) acc
      else Math.max(
        loop(r.left, acc + 1),
        loop(r.right, acc + 1))
    }

    loop(t, -1)
  }

  def levelTraversal(t: Node): Unit = {

    val queue = new scala.collection.mutable.Queue[Node]()
    queue.enqueue(t)

    while (!queue.isEmpty) {
      val node = queue.dequeue()
      if (node != null) {
        print(node.data + " ")
        queue.enqueue(node.left, node.right)
      }
    }

    println()
  }

  def encode(t: Node): Array[Array[Int]] = {
    import scala.collection.mutable.ArrayBuffer
    val buff = new ArrayBuffer[Array[Int]]()

    def leafsAppend(r: Node): Unit = {
      buff.append(Array(
        if (r.left == null) -1 else r.left.data,
        if (r.right == null) -1 else r.right.data
      ))
    }

    val queue = new scala.collection.mutable.Queue[Node]()
    if (t != null) queue.enqueue(t)
    while (!queue.isEmpty) {
      val node = queue.dequeue()
      leafsAppend(node)
      if (node.left != null) queue.enqueue(node.left)
      if (node.right != null) queue.enqueue(node.right)
    }

    buff.toArray
  }

  def decode(nodes: Array[Array[Int]]): Node = {
    var index = 0
    val queue = new scala.collection.mutable.Queue[Node]()

    val root = Node(1)
    queue.enqueue(root)

    while (index < nodes.length && !queue.isEmpty) {
      val r = queue.dequeue()

      r.left = if (nodes(index)(0) == -1) null else Node(nodes(index)(0))
      r.right = if (nodes(index)(1) == -1) null else Node(nodes(index)(1))

      if (r.left != null) queue.enqueue(r.left)
      if (r.right != null) queue.enqueue(r.right)

      index = index + 1
    }

    root
  }

  def inOrderTraverse(t: Node): Array[Int] = {
    def loop(r: Node): Seq[Int] = {
      if (r == null) Nil
      else inOrderTraverse(t.left) ++ Seq(r.data) ++ inOrderTraverse(r.right)
    }

    loop(t).toArray
  }

  def swapTree(t: Node, filterLevels: Array[Int]): Node = {
    var level = 1: Int

    val queue = new scala.collection.mutable.Queue[Node]()
    if (t != null) queue.enqueue(t)

    while (queue.nonEmpty) {
      val nodes = queue.dequeueAll(_ => true)
      if (filterLevels.contains(level)) {
        println(s"----- $level -- $nodes")
        nodes foreach { n =>
          val temp = n.left
          n.left = n.right
          n.right = temp
        }
      }
      nodes foreach (n => {
        if (n.left != null) queue.enqueue(n.left)
        if (n.right != null) queue.enqueue(n.right)
      })
      level = level + 1
    }
    t
  }

  def swapNodes(indexes: Array[Array[Int]], queries: Array[Int]): Array[Array[Int]] = {
    // Write your code here
    val node = decode(indexes)
    println(inOrderTraverse(node).toList)

    val list = for (i <- 0 until queries.length)
      yield inOrderTraverse(swapTree(node, queries.drop(i)))

    list.toArray
  }
}


case class Node(data: Int, var left: Node = null, var right: Node = null)
