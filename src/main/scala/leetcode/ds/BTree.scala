package leetcode.ds

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class TreeNode(value: Int, var left: TreeNode = null, var right: TreeNode = null)

case class BTreeNode(value: Int, left: BTreeNode = null, right: BTreeNode = null, var next: BTreeNode = null)


object BTree extends App {

  /*
  Top-Down: (~ tail recursive)
    operate on current node;
      use its values while calling recursive function recursively;

  Bottom-up: (~ non-tail recursive)
    call the recursive function first;
      then use its return value & operate on current node
   */


  var treeNode = TreeNode(6,
    TreeNode(21,
      TreeNode(1),
      TreeNode(4, TreeNode(-31), TreeNode(5))),
    TreeNode(7, right = TreeNode(9, left = TreeNode(8))))

  var treeNode2 = TreeNode(1,
    TreeNode(2, TreeNode(3), TreeNode(4)),
    TreeNode(2, TreeNode(4), TreeNode(3)))


  var inOrder = Array(9, 3, 15, 20, 7)
  var postOrder = Array(9, 15, 7, 20, 3)
  val preorder = Array(3, 9, 20, 15, 7)

  val btree = BTreeNode(1,
    BTreeNode(2, BTreeNode(4), BTreeNode(5)),
    BTreeNode(3, BTreeNode(6), BTreeNode(7))
  )

  var btreeNonPerfect = BTreeNode(1,
    BTreeNode(2, BTreeNode(4), BTreeNode(5)),
    BTreeNode(3, right = BTreeNode(7)))

  {

    //4-7-3####-9-39#6-706#-1#####-4######-6-65######9#-2########-4######
    //[4,-7,-3,null,null,-9,-3,9,-7,-4,null,6,null,-6,-6,null,null,0,6,5,null,9,null,null,-1,-4,null,null,null,-2]
    val t1 = TreeNode(4,
      left = TreeNode(-7),
      right = TreeNode(-3,
        left = TreeNode(-9,

          left = TreeNode(9,
            left = TreeNode(6, left = TreeNode(0, right = TreeNode(-1)),
              right = TreeNode(6, left = TreeNode(-4))
            )
          ),
          right = TreeNode(-7,
            left = TreeNode(-6, TreeNode(5)),
            right = TreeNode(-6, right = TreeNode(-6,
              left = TreeNode(9, left = TreeNode(-2)))))

        ),
        right = TreeNode(-3, left = TreeNode(-4)))
    )

    println(t1)
    val s = serialize(t1)
    println("Serialized: " + s)
    println(deserialize(s))

    val s2 = serialize(null)
    println("Serialized: " + s2)
    println(deserialize(s2))
  }

  // Encodes a list of strings to a single string.
  def serialize(root: TreeNode): String = {
    def loop(t: TreeNode): String = {
      if (t != null) {
        (if (root == t) s"${t.value.toString}," else "") concat
          (if (t.left != null) s"${t.left.value.toString}," else "#,") concat
          (if (t.right != null) s"${t.right.value.toString}," else "#,") concat
          loop(t.left) concat
          loop(t.right)
      } else ""
    }
    loop(root)

    /*var acc = ""
    def loop2(t: TreeNode): Unit = {
      if (t != null) {
        acc = acc concat
          (if (t.left != null) s"${t.left.value.toString}," else "#,") concat
          (if (t.right != null) s"${t.right.value.toString}," else "#,")

        loop2(t.left)
        loop2(t.right)
      }
    }
    if (root == null) "" else {
      acc = s"${root.value},"
      loop2(root)
      acc
    }*/
  }

  // Decodes a single string to a list of strings.
  def deserialize(data: String): TreeNode = {
    var index = 0

    def readNext: String = {
      var str = data(index).toString
      index = index + 1

      while (index < data.length && data(index) != ',') {
        str = str concat data(index).toString
        index = index + 1
      }

      index = index + 1
      str
    }


    def loop2(node: TreeNode): TreeNode = {
      if (index < data.length) {

        var s = readNext
        if (s != "#") node.left = TreeNode(s.toInt)

        s = readNext
        if (s != "#") node.right = TreeNode(s.toInt)

        if (node.left != null) loop2(node.left)
        if (node.right != null) loop2(node.right)
      }

      node
    }

    if (data.nonEmpty) loop2(node = TreeNode(readNext.toInt)) else null

    /*lazy val dataArray = data.split(",")
    def loop(node: TreeNode): TreeNode = {
      if (index < dataArray.length) {
        if (dataArray(index) != "#") node.left = TreeNode(dataArray(index).toInt)
        index = index + 1

        if (index < dataArray.length && dataArray(index) != "#") {
          node.right = TreeNode(dataArray(index).toInt)
        }
        index = index + 1

        if (node.left != null) loop(node.left)
        if (node.right != null) loop(node.right)
      }
      node
    }
    if (data.nonEmpty) loop(node = TreeNode(dataArray(0).toInt)) else null
    */
  }


  println("-------lowestCommonAncestor--------")
  println(lowestCommonAncestor(treeNode, TreeNode(3), TreeNode(9, left = TreeNode(8))))
  println(lowestCommonAncestor(treeNode, TreeNode(3), TreeNode(5)))
  println("-------lowestCommonAncestor--------")

  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    var finalNode: TreeNode = null

    def loop(node: TreeNode): (Boolean) = {
      if (node == null) false
      else {
        val l = if (loop(node.left)) 1 else 0
        val r = if (loop(node.right)) 1 else 0
        val self = if (p == node || q == node) 1 else 0

        if (l + r + self >= 2) finalNode = node
        l + r + self > 0
      }
    }

    loop(root)
    finalNode
  }


  println(connect(btree))
  println(connectRec(btree))
  println(connectRecNonPerfect(btreeNonPerfect))

  def connectRecNonPerfect(root: BTreeNode): BTreeNode = {

    def loop(node: BTreeNode): Unit = {
      if (node == null) return

      var first = null: BTreeNode

      var prev = null: BTreeNode
      var t = node
      do {
        if (t.left != null) {
          if (prev != null) prev.next = t.left else first = t.left
          prev = t.left
        }
        if (t.right != null) {
          if (prev != null) prev.next = t.right else first = t.right
          prev = t.right
        }
        t = t.next
      } while (t != null)

      loop(first)
    }

    loop(root)
    root
  }

  def connectRec(root: BTreeNode): BTreeNode = {

    def loop(node: BTreeNode): Unit = {
      if (node.left != null) {
        node.left.next = node.right

        var t = node
        while (t.next != null) {
          t.right.next = t.next.left
          t.next.left.next = t.next.right
          t = t.next
        }
        loop(node.left)
      }
    }

    if (root == null) root else loop(root)
    root
  }

  def connect(root: BTreeNode): BTreeNode = {

    if (root == null) return root

    val ds = new mutable.Stack[BTreeNode]()
    ds.push(root)

    while (ds.nonEmpty) {
      var lastNode = null: BTreeNode

      val visitedNodes = new mutable.Stack[BTreeNode]()
      while (ds.nonEmpty) {
        val node = ds.pop()
        node.next = lastNode
        lastNode = node

        if (lastNode.right != null) {
          visitedNodes.push(lastNode.right)
          visitedNodes.push(lastNode.left)
        }
      }
      ds.pushAll(visitedNodes)
    }

    root
  }


  println(buildTree(inOrder, postOrder))
  println(buildTree(List(1, 2, 3, 4, 5, 6, 7, 8, 9).toArray, List(1, 3, 5, 4, 2, 8, 9, 7, 6).toArray))

  println(buildTreePreIn(preorder, inOrder))
  println(buildTreePreIn(List(6, 2, 1, 4, 3, 5, 7, 9, 8).toArray, List(1, 2, 3, 4, 5, 6, 7, 8, 9).toArray))


  def buildTreePreIn(preorder: Array[Int], inorder: Array[Int]): TreeNode = {

    def loop(s: Int, e: Int, sP: Int, eP: Int): TreeNode = {
      val root = preorder(sP)

      var splitAt = s
      while (root != inorder(splitAt)) splitAt = splitAt + 1

      TreeNode(root,
        left = if (splitAt == s) null else loop(s, splitAt - 1, sP + 1, sP + 1 + (splitAt - 1 - s)),
        right = if (splitAt == e) null else loop(splitAt + 1, e, sP + 1 + (splitAt - 1 - s) + 1, eP)
      )
    }

    loop(0, inorder.length - 1, 0, inorder.length - 1)
  }

  def buildTree(inorder: Array[Int], postorder: Array[Int]): TreeNode = {

    def loop(s: Int, e: Int, sP: Int, eP: Int): TreeNode = {
      val root = postorder(eP)
      val splitAt = {
        var i = s;
        while (inorder(i) != root) i = i + 1
        i
      }

      TreeNode(inorder(splitAt),
        left = if (splitAt - 1 < s) null else loop(s, splitAt - 1, sP, sP + (splitAt - 1 - s)),
        right = if (splitAt + 1 > e) null else loop(splitAt + 1, e, sP + (splitAt - 1 - s) + 1, eP - 1))
    }

    loop(0, inorder.length - 1, 0, inorder.length - 1)
  }

  def buildTreeWrong(inorder: Array[Int], postorder: Array[Int]): TreeNode = {

    var totalRoots = 0

    def sub(startIndex: Int = 0, endIndex: Int = inorder.length): TreeNode = {
      var index = startIndex
      var tree = null: TreeNode
      println(startIndex + " " + endIndex)

      while (index < endIndex) {
        println(tree)
        if (tree == null) {
          tree = TreeNode(inorder(index))
          index = index + 1
        } else if (tree.left == null) {
          tree = TreeNode(inorder(index), left = tree)
          index = index + 1
        } else if (tree.right != null) {
          tree = TreeNode(inorder(index), left = tree)
          index = index + 1
          totalRoots = totalRoots + 1
        } else {
          var left = 0
          while (postorder(left) != inorder(index - 2)) {
            left = left + 1
          }

          var diff = 0
          while (inorder(index - 1) != postorder(left + 1 + diff)) {
            println(inorder(index - 1) + "  " + postorder(index - 1 - totalRoots + diff) + " " + diff + " " + tree)
            diff = diff + 1
          }

          tree = tree.copy(right =
            if (diff == 1) TreeNode(inorder(index))
            else sub(index, index + diff))
          index = index + (diff + 1)
        }
      }
      tree
    }

    sub()
  }


  println(hasPathSum(treeNode, 30))
  println(hasPathSum(TreeNode(1, TreeNode(2)), 1))

  println(isSymmentric(treeNode2))
  println(isSymmentric(treeNode))
  println(isSymmentric2(treeNode2))
  println(isSymmentric2(treeNode))

  println(maxDepth(treeNode))
  println(maxDepth2(treeNode))
  println(maxDepth3(treeNode))

  println(bfs(treeNode))

  println(postorder(treeNode))
  println(postorder1(treeNode))
  println(postOrderRecursive(treeNode))
  println(postOrderTailRecursive(treeNode))

  println(preorder(treeNode))
  println(preOrderRecursive(treeNode))
  println(preOrderTailRecursive(treeNode))

  println(inorder(treeNode))
  println(inOrderRecursive(treeNode))
  println(inOrderTailRecursive(treeNode))


  def hasPathSum(root: TreeNode, targetSum: Int): Boolean = {

    def sumTillLeaf(t: TreeNode, acc: Int): Boolean = {
      if (t == null) false
      else {
        val newAcc = acc + t.value
        if (t.left == null && t.right == null) targetSum == newAcc
        else sumTillLeaf(t.left, newAcc) || sumTillLeaf(t.right, newAcc)
      }
    }

    sumTillLeaf(root, 0)
  }


  def isSymmentric(node: TreeNode): Boolean = {

    def compare(l: TreeNode, r: TreeNode): Boolean = {
      if (l == null && r == null) true
      else {
        (l != null && r != null) &&
          l.value == r.value &&
          compare(l.left, r.right) &&
          compare(l.right, r.left)
      }
    }

    if (node == null) true else compare(node.left, node.right)
  }

  def isSymmentric2(node: TreeNode): Boolean = {
    val ds = new mutable.Stack[TreeNode]()
    ds.push(node)
    ds.push(node)

    var result = true
    while (ds.nonEmpty && result) {
      val l = ds.pop()
      val r = ds.pop()

      def bothNull = l == null && r == null

      lazy val noneNull = l != null && r != null

      def bothSameValue = l.value == r.value

      result = bothNull || (noneNull && bothSameValue)

      if (noneNull) {
        ds.push(l.right)
        ds.push(r.left)

        ds.push(l.left)
        ds.push(r.right)
      }
    }

    result
  }

  def maxDepth(node: TreeNode): Int = {

    def loop(node: TreeNode, depth: Int): Int = {
      if (node == null) depth
      else Math.max(loop(node.left, depth + 1), loop(node.right, depth + 1))
    }

    loop(node, 0)
  }

  def maxDepth2(node: TreeNode): Int = {
    var max = 0: Int

    def loop(node: TreeNode, depth: Int): Unit = {
      max = Math.max(max, depth + 1)

      if (node.left != null) {
        loop(node.left, depth + 1)
      }
      if (node.right != null) {
        loop(node.right, depth + 1)
      }
    }

    if (node == null) 0 else loop(node, 0)
    max
  }

  def maxDepth3(node: TreeNode): Int = {
    if (node == null) {
      println("--null")
      0
    }
    else {
      println(s"--${
        node.value
      }")
      Math.max(maxDepth3(node.left), maxDepth3(node.right)) + 1
    }
  }

  /*def maxDepth(node: TreeNode): Int = {

    def loop(t: TreeNode, acc: Int): Int = {
      if (t.left == null && t.right == null) acc
      else
    }

    if (node == null) 0 else loop(node, 1)
  }*/

  //post-order with visited mark & re-push of root wit left/right
  def postorder1(b: TreeNode): List[Int] = {
    if (b == null) return Nil
    val l = new ListBuffer[Int]()
    val set = new mutable.HashSet[TreeNode]()

    var t = b
    val ds = new mutable.Stack[TreeNode]()
    ds.push(t)
    while (ds.nonEmpty) {
      t = ds.pop()
      if (t.left != null && !set.contains(t.left)) {
        ds.push(t)
        ds.push(t.left)
      } else if (t.right != null && !set.contains(t.right)) {
        ds.push(t)
        ds.push(t.right)
      } else {
        l.append(t.value)
        set.add(t)
      }
    }

    l.toList
  }

  //self,left,right
  def preorder(b: TreeNode): List[Int] = {

    val l = new ListBuffer[Int]()

    val ds = new mutable.Stack[TreeNode]()
    var t = b

    def opearteLeft =
      while (t != null) {
        l.+=(t.value)
        ds.push(t)
        t = t.left
      }

    opearteLeft
    while (!ds.isEmpty) {
      t = ds.pop().right
      opearteLeft
    }

    l.toList
  }

  //left,self,right
  def inorder(b: TreeNode): List[Int] = {

    val l = new ListBuffer[Int]()

    val ds = new mutable.Stack[TreeNode]()
    var t = b

    def operateLeft =
      while (t != null) {
        ds.push(t)
        t = t.left
      }

    operateLeft
    while (!ds.isEmpty) {
      t = ds.pop()
      l.+=(t.value)
      t = t.right
      operateLeft
    }

    l.toList
  }

  //left,right,self
  def postorder(b: TreeNode): List[Int] = {
    if (b == null) return Nil

    val l = new ListBuffer[Int]()
    val ds = new mutable.Stack[TreeNode]()

    var node = b
    ds.push(b)
    while (ds.nonEmpty) {
      node = ds.pop();
      l.+=(node.value)

      if (node.left != null) {
        ds.push(node.left)
      }
      if (node.right != null) {
        ds.push(node.right)
      }
    }

    l.reverse.toList
  }


  def postOrderRecursive(t: TreeNode): List[Int] = {
    if (t == null) Nil
    else postOrderRecursive(t.left) ++ postOrderRecursive(t.right) ++ List(t.value)
  }

  def preOrderRecursive(t: TreeNode): List[Int] = {
    if (t == null) Nil
    else List(t.value) ++ preOrderRecursive(t.left) ++ preOrderRecursive(t.right)
  }

  def inOrderRecursive(t: TreeNode): List[Int] = {
    if (t == null) Nil
    else inOrderRecursive(t.left) ++ List(t.value) ++ inOrderRecursive(t.right)
  }


  def postOrderTailRecursive(t: TreeNode): List[Int] = {
    def loop(acc: List[Int], t2: TreeNode): List[Int] = {
      if (t2 == null) acc
      else {
        val al = loop(acc, t2.left)
        val ar = loop(al, t2.right)
        loop(ar ++ List(t2.value), null)
      }
    }

    loop(Nil, t)
  }

  def preOrderTailRecursive(t: TreeNode): List[Int] = {
    def loop(acc: List[Int], t2: TreeNode): List[Int] = {
      if (t2 == null) acc
      else {
        val aro = loop(acc ++ List(t2.value), null)
        val al = loop(aro, t2.left)
        val ar = loop(al, t2.right)
        ar
      }
    }

    loop(Nil, t)
  }

  def inOrderTailRecursive(t: TreeNode): List[Int] = {
    def loop(acc: List[Int], t2: TreeNode): List[Int] = {
      if (t2 == null) acc
      else {
        val al = loop(acc, t2.left)
        val aro = loop(al ++ List(t2.value), null)
        val ar = loop(aro, t2.right)
        ar
      }
    }

    loop(Nil, t)
  }

  def bfs(t: TreeNode): List[List[Int]] = {
    val l = new ListBuffer[List[Int]]()
    val q = mutable.Queue[TreeNode]();
    q.enqueue(t)

    while (q.nonEmpty) {
      val l2 = q.dequeueAll(_ => true)
        .map(node => {
          if (node.left != null) q.enqueue(node.left);
          if (node.right != null) q.enqueue(node.right);
          node.value
        })
      l.append(l2.toList)
    }

    l.toList
  }

  def levelOrder(root: TreeNode): List[List[Int]] = {
    if (root == null) return Nil

    val l = new ListBuffer[List[Int]]()
    var q = mutable.Queue[TreeNode]();
    q.enqueue(root)

    while (q.nonEmpty) {
      val qTemp = mutable.Queue[TreeNode]()

      val l2 = new ListBuffer[Int]
      while (q.nonEmpty) {
        val node = q.dequeue()
        l2.append(node.value)
        if (node.left != null) qTemp.enqueue(node.left);
        if (node.right != null) qTemp.enqueue(node.right);
      }
      l.append(l2.toList)
      q = qTemp
    }

    l.toList
  }

  /*
   6 2 | 1
   6 | 2 | 1
   6 4 | 2 | 1 3
   6 | 2 4 | 1 3
   6 | 2 4 | 1 3 5
   stack-size diff
   6 | 2 | 1354
   6 | | 13542
   | 6 | 13542
   7 | 6 | 13542
   7 | 6 | 13542

   */
}
