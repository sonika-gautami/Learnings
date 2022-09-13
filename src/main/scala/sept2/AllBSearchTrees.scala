package sept2

case class TreeNode(var data: Int, var left: TreeNode = null, var right: TreeNode = null)

object AllBSearchTrees extends App {

  println(generateTrees(1))
  println(generateTrees(2))
  println(generateTrees(2).length)
  println(generateTrees(3))
  println(generateTrees(3).length)
  println(generateTrees(4))
  println(generateTrees(4).length)
  println(numTrees(1))
  println(numTrees(2))
  println(numTrees(3))
  println(numTrees(4))

  /*
  Binary Search Tree =  Binary tree with:
                          - left subtree values < right
                          - right subtree values > left
                          - left & right of a node is also a binary search tree
   */
  def generateTrees(n: Int): List[TreeNode] = {

    def loop(start: Int, end: Int): List[TreeNode] = {
      val all = start to end flatMap { i =>
        val root = i

        val lefts: Seq[TreeNode] = loop(start, i - 1)
        val rights: Seq[TreeNode] = loop(i + 1, end)

        val nodes: Seq[TreeNode] = {
          if (lefts.isEmpty && rights.isEmpty) {
            List(TreeNode(root))
          }
          else if (lefts.isEmpty) {
            for (j <- 0 until rights.length)
              yield TreeNode(root, null, rights(j))
          } else if (rights.isEmpty) {
            for (i <- 0 until lefts.length)
              yield TreeNode(root, lefts(i), null)
          } else {
            for (i <- 0 until lefts.length; j <- 0 until rights.length)
              yield TreeNode(root, lefts(i), rights(j))
          }
        }
        nodes
      }
      all.toList
    }

    loop(1, n)
  }


  def numTrees(n: Int): Int = {
    if (n <= 0) 0
    else if (n == 1) 1
    else {
      var acc = 0
      for (i <- 1 to n) {
        val lefts = if (i - 1 < 1) 0 else numTrees(i - 1)
        val rights = if (i - 1 == n - i) lefts else if (n - i < 1) 0 else numTrees(n - i)

        acc = acc + {
          if (lefts == 0) rights
          else if (rights == 0) lefts
          else lefts * rights
        }
      }
      acc
    }
  }

  def recoverTreeOnlyIfConnected(root: TreeNode): Unit = {

    def dfs(t: TreeNode): Boolean = {
      if (t == null) false
      else if (t.left != null && t.data < t.left.data) {
        val temp = t.left.data
        t.left.data = t.data
        t.data = temp
        true
      } else if (t.right != null && t.data > t.right.data) {
        val temp = t.right.data
        t.right.data = t.data
        t.data = temp
        true
      } else {
        if (!dfs(t.left)) dfs(t.right) else true
      }
    }

    dfs(root)
  }

  def recoverTree(root: TreeNode): Unit = {

  }
}
