package coursera.functional.design.week1

import scala.util.control.NonFatal


/*

Monad M =>
 Parametric Type M[T]

 Two Operations:
 1. flatMap (bind)
 2. unit

 Monad Laws:
 1. Associativity
 2. Left Unit
 3. Right Unit

If Monad also defines the 'withFilter'
then, it is called as 'Monads with Zero'

trait M[T] {

  def flatMap[U](f: T => M[U]): M[U]

  List[Int] (M = List, T = Int, U = String) (f = i => List(i.toString)]) -> flatMap(1) = List("1")

  def unit[T](v: T): M[T]

  List[Int]   (M = List,  T = Int) -> unit(1) = List(1)
  Set[Int]    (M = Set,   T = Int) -> unit(1) = Set(1)
  Option[Int] (M = Option,T = Int) -> unit(1) = Some(1)
  Option[Int] (M = Option,T = Int) -> unit(1) = Some(1)

  //def map[U]: M[U] = flatMap(f: T => M[U])

  map as function f of every Monad ->

  m map(f) = m flatMap ( v => unit(f(v) )
           ~ m flatMap ( f andThen unit )

  i.e
  List[Int]   (M = List,  T = Int) -> unit(1) = List(1)
  U = String
  flatMap[String](i : Int => List[String])

  map = flatMap[String](i : Int => List[String]) andThen unit[]

  Monad Laws:

  m flatMap f flatMap g == m flatMap(x => f(x) flatMap g)
  unit(x) flatMap f == f(x)
  m flatMap unit == m
}

abstract class Option_[+T] {

  def flatMap[U](f: T => Option[U]): Option[U] = Option(_) this match {
    case Some(a) => f(a)
    case None => None[U]
  }

  def unit[T](v: T): Option[T] = if (v == null) None[T] else Some(v)

  //def unit = Some(v)


  //Left Unit Low
  //unit(x) flatMap f == f(x)
  val x: Int = 1
  val f: Int => Option[String] = (i: Int) => if (i < 0) None[String] else Some("Natural Num")
  val leftSide_UnitLeftLaw: Option[String] = unit(x).flatMap(f)
  val rightSide_UnitLeftLaw: Option[String] = f(x)

  //Right Unit Law
  //m flatMap unit == m
  val m = Option(x)
  val leftSide_UnitRightLaw = m.flatMap(f)
  .unit(x)
  val rightSide_UnitRightLaw = m

  //Associativity Law
  //m flatMap f flatMap g == m flatMap(x => f(x) flatMap g)
  val g: String => Option[Long] = (s: String) => if (s.length > 8) None[Long] else Some(s.toLong)
  val leftSide_AssociativityLaw: Option[Long] = m.flatMap(f).flatMap(g)
  val rightSide_AssociativityLaw: Option[Long] = m.flatMap(x => f(x).flatMap(g))

}
*/
