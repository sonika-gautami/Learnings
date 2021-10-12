package leetcode.ds

case class Tree_(self: Int, left: Tree_ = null, right: Tree_ = null)

object BTreeTraversalRecursion extends App {

  /*

List(1, 21, -31, 4, 5, 6, 7, 8, 9)
  List(1, -31, 5, 4, 21, 8, 9, 7, 6)
   */
  var treeNode = Tree_(6,
    Tree_(21,
      Tree_(1),
      Tree_(4, Tree_(-31), Tree_(5))),
    Tree_(7, right = Tree_(9, left = Tree_(8))))
  println(prefix(treeNode)) //List(6, 21, 1, 4, -31, 5, 7, 9, 8)
  println(infix(treeNode)) //List(1, 21, -31, 4, 5, 6, 7, 8, 9)
  println(postfix(treeNode)) //List(1, -31, 5, 4, 21, 8, 9, 7, 6)

  println(bfs(treeNode))  //6 21 7 1 4 9 -31 5 8

  def bfs(t: Tree_): String = {
    def loop(childs: List[Tree_]): String = {
      if (childs == Nil) ""
      else {
        childs.map(_.self + " ").mkString +
          loop(
            childs.flatMap(t => List(t.left, t.right)).filter(_ != null)
          )
      }
    }

    loop(List(t))
  }

  def prefix(t: Tree_): String = {
    if (t == null) ""
    else t.self + " " + prefix(t.left) + prefix(t.right)
  }

  def infix(t: Tree_): String = {
    if (t == null) ""
    else infix(t.left) + t.self + " " + infix(t.right)
  }

  def postfix(t: Tree_): String = {
    if (t == null) ""
    else postfix(t.left) + postfix(t.right) + t.self + " "
  }
}
