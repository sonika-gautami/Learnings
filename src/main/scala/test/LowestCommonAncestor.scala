package test

import leetcode.ds.TreeNode

import java.util

object LowestCommonAncestor extends App {
  /*
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

   */

  var treeNode = TreeNode(6,
    TreeNode(21,
      TreeNode(1),
      TreeNode(4, TreeNode(-31), TreeNode(5))),
    TreeNode(7, right = TreeNode(9, left = TreeNode(8))))
  println(preOrder(treeNode))
  println(bfs(treeNode))

  def lca(n: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    var finalNode: TreeNode = null

    def nodeHasBoth(t: TreeNode): Boolean = {
      if (t == null) false
      else if (finalNode != null) false
      else {
        val self = if (t == p || t == q) 1 else 0
        val left = if (nodeHasBoth(t.left)) 1 else 0
        if (self + left == 2) {
          finalNode = t
          true
        } else {
          val right = if (nodeHasBoth(t.right)) 1 else 0
          if (self + left + right >= 2) {
            finalNode = t
            true
          } else self + left + right > 0
        }
      }
    }
    nodeHasBoth(n)
    finalNode
  }

  def bfs(n: TreeNode): String = {
    def loop(t: List[TreeNode], acc: String): String = {
      val nonNull = t.filter(_ != null)
      if (nonNull.isEmpty) acc
      else {
        val newAcc = acc + nonNull.map(_.value.toString).mkString(" , ") + " , "
        loop(
          nonNull.flatMap(tn => List(tn.left, tn.right)),
          newAcc)
      }
    }

    loop(List(n), "")
  }

  def preOrder(n: TreeNode): String = {
    def loop(t: TreeNode, acc: String): String = {
      if (t == null) acc
      else {
        val newAcc = loop(t.left, acc + ", " + t.value.toString)
        loop(t.right, newAcc)
      }
    }

    loop(n, "")
  }

  println("-------lowestCommonAncestor--------")
  println(lowestCommonAncestor1(treeNode, TreeNode(3), TreeNode(9, left = TreeNode(8))))
  println(lca(treeNode, TreeNode(3), TreeNode(9, left = TreeNode(8))))
  println(lowestCommonAncestor1(treeNode, TreeNode(8), TreeNode(5)))
  println(lca(treeNode, TreeNode(8), TreeNode(5)))
  println("-------lowestCommonAncestor--------")

  def lowestCommonAncestor1(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    var finalNode: TreeNode = null

    def loop(n: TreeNode): Boolean = {
      if (n != null) {
        val c = (if (loop(n.left)) 1 else 0) +
          (if (loop(n.right)) 1 else 0) +
          (if (n == p || n == q) 1 else 0)
        if (c >= 2) finalNode = n
        c >= 0
      }
      else false
    }

    loop(root)
    finalNode
  }

  def lowestCommonAncestor2(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    var foundCount = 0

    def loop(n: TreeNode): TreeNode = {
      if (foundCount == 2) return n
      else if (n != null) {
        loop(n.left)
        loop(n.right)
        if (n == p || n == q) foundCount = foundCount + 1
      }
      null
    }

    loop(root)
  }

  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    val stack = new util.Stack[TreeNode]()
    stack.push(root)

    var foundCount = 0
    var finalNode: TreeNode = null

    while (!stack.isEmpty && finalNode == null) {
      val node = stack.pop()
      if (node != null) {
        if (foundCount == 2) finalNode = node

        stack.push(node.left)
        stack.push(node.right)
        if (node == p || node == q) foundCount = foundCount + 1
      }
    }

    if (foundCount < 2) null else finalNode
  }

}