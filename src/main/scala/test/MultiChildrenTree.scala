package test

import scala.collection.mutable.ListBuffer

object MultiChildrenTree extends App {
  val levels = Seq(
    Levels("a1", "a2", "a3"),
    Levels("a1", "a2", "a4"),
    Levels("b1", "b2", "b3"),
    Levels("b1", "b5", "b6"),
    Levels("c1", "c2"),
    Levels("d1"),
    Levels("e1", "e2", "e3", "e4", "e5"),
    Levels("e1", "e2", "e3", "e6"),
    Levels("e1", "e2", "e7"),
    Levels("e1", "e2", "e9", "e10")
  )

  def levelsToTree(levels: Seq[Levels], levelUpTo: Int): Tree = {

    def loop(tree: Tree, remainingLevels: Seq[Levels]): Tree =
      if (remainingLevels.isEmpty) tree
      else {
        val l = remainingLevels.head
        val t = tree.upsert(
          (1 to levelUpTo).map(l.valueAtLevel(_))
        )
        loop(t, remainingLevels.tail)
      }

    loop(Leaf("ROOT"), levels)
  }

  def testRuns(l: Int) = {
    val tree = levelsToTree(levels, l)
    println(tree)

    val strList = tree.traverse
    println(strList)
  }

  testRuns(3)
  testRuns(4)
  testRuns(5)
}


trait Tree {
  def self: String

  def children: ListBuffer[Tree]

  private def upsertInternal(node: Tree, s: String): (Tree, Boolean) =
    node.children.find(t => t.self == s).map(t => t -> false).getOrElse(Leaf(s) -> true)

  def upsert(list: Seq[String], tree: Tree = this): Tree = {

    def loop(node: Tree, remainings: Seq[String]): Tree =
      if (remainings.isEmpty || remainings.head.isEmpty) node
      else {
        val (t, isNew) = upsertInternal(node, remainings.head)
        if(isNew) node.children.append(t)

        loop(t, remainings.tail)
      }

    loop(tree, list)
    tree
  }

  def traverse: Seq[String] = {
    def path(tree: Tree, all: Seq[String]): Seq[String] = {
      //println("------------------")
      //println(tree)
      //println(all)
      if (tree.children.isEmpty) all
      else {
        tree.children.flatMap(t => {
          path(t, all.map(s =>  s + "/" + t.self))
        })
      }
    }
    path(this, Seq(""))
  }
}

case class Parent(self: String,
                  children: ListBuffer[Tree]) extends Tree

case class Leaf(self: String,
                children: ListBuffer[Tree] = ListBuffer()) extends Tree


trait Traversable {
  def valueAtLevel(n: Int): String
}

case class Levels(l1: String,
                  l2: String = "",
                  l3: String = "",
                  l4: String = "",
                  l5: String = "") extends Traversable {
  override def valueAtLevel(n: Int): String = n match {
    case 1 => l1
    case 2 => l2
    case 3 => l3
    case 4 => l4
    case 5 => l5
  }
}