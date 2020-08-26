package coursera.functional.design.week1

import scala.util.Random

trait Generator[+T] {
  self => //alias name

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    //Here, f(this.generate) | f(generate) will do a recursive call
    //Using alias-name(self) self.generate refers to the outer trait's generate method.
    override def generate: S = f(self.generate) //use of alias
  }

  def flatMap[S](f: T => Generator[S]) = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }
}


object Test extends App {
  println(RandomGenerators.booleans.generate)

  println(RandomGenerators.integers.generate)

  println(RandomGenerators.pairs.generate)

  println(RandomGenerators.lists.generate)

  println(RandomGenerators.treeMap.generate)


  test(RandomGenerators.pairsOfList) {
    case (l1: List[Int], l2: List[Int]) => (l1 ++ l2).length > l1.length
  }


  def test[T](generator: Generator[T], numTimes: Int = 10)
             (f: T => Boolean): Unit = {

    (0 until numTimes).foreach { _ =>
      val anyVal = generator.generate
      assert(f(anyVal), s"failed for $anyVal")
    }
  }
}


trait Tree

case class Leaf(x: Int) extends Tree

case class Inner(l: Tree, r: Tree) extends Tree


object RandomGenerators {

  val integers = new Generator[Int] {
    override def generate: Int = Random.nextInt()
  }

  val booleansInFor = for {i <- integers} yield (i > 0)
  val booleans: Generator[Boolean] = integers.map(_ > 0)


  val pairsOfList: Generator[(List[Int], List[Int])] = for {
    x <- lists
    y <- lists
  } yield (y -> x)

  val pairs: Generator[(Int, Int)] = for {
    x <- integers
    y <- integers
  } yield (y -> x)
  val pairsMap: Generator[(Int, Int)] =
    integers.flatMap(x => {
      integers.map(y => y -> x)
    })

  val emptyLists: Generator[List[Int]] = for (i <- integers) yield (Nil: List[Int])
  val emptyListMap = integers.map(_ => Nil: List[Int])


  val nonEmptyLists: Generator[List[Int]] = for {
    head <- integers
    tail <- lists
  } yield (head :: tail)

  val nonEmptyListsMap: Generator[List[Int]] =
    integers.flatMap(head => {
      lists.map(tail => head :: tail)
    })

  lazy val lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    l <- if (isEmpty) emptyLists else nonEmptyLists
  } yield l
  val listsWithMap: Generator[List[Int]] =
    booleans.flatMap { isEmpty =>
      (if (isEmpty) emptyLists else nonEmptyLists)
    }


  val leaf: Generator[Leaf] = for {i <- integers} yield Leaf(i)
  val leafMap = integers.map(Leaf(_))

  val tree: Generator[Tree] = for {
    isEmpty <- booleans
    tree <- if (isEmpty) leaf else inner
  } yield tree
  val treeMap: Generator[Tree] =
    booleans.flatMap { isEmpty =>
      if (isEmpty) leaf else inner
    }

  lazy val inner: Generator[Inner] = for {
    l <- tree
    r <- tree
  } yield Inner(l, r)

  val innerMap: Generator[Inner] = tree.flatMap { l =>
    tree.map(r => Inner(l, r))
  }
}