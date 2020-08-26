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
*/

abstract class Try_[+T] {

  def flatMap[U](f: T => Try_[U]): Try_[U] = this match {
    case Success_(x) =>
      try f(x)
      catch {
        case NonFatal(e) => Failure_(e)
      }
    case f: Failure_[Exception] => f
  }

  def map[U](f: T => U): Try_[U] = this match {
    case Success_(x) => Try_(f(x))
    case f: Failure_[Exception] => f
  }

  //Monad : Unit function
  def unit[T](x: T): Try_[T] = Try_(x)

  //Monad : Map function from flatMap & Unit
  def mapWithFlatMap_Unit[U](f: T => U): Try_[U] = this.flatMap(x => unit(f(x)))

  //Monad : Map function from flatMap & Unit
  def mapWithFlatMap_Unit2[U](f: T => U): Try_[U] =
    this.flatMap(f andThen unit) //this.flatMap(f.andThen(unit))
}

case class Success_[T](v: T) extends Try_[T]

case class Failure_[Exception](e: Exception) extends Try_[Nothing]

object Try_ {
  def apply[T](f: => T): Try_[T] =
    try {
      Success_(f)
    } catch {
      case NonFatal(e) => Failure_(e)
    }
}

object Lec_1_4_Try extends App {

  val s = Try_({
    println("From Try block")
    "Success"
  })
  println(s"$s -> ${s.getClass}")


  val f = Try_({
    10 / 0
  })
  println(s"$f -> ${f.getClass}")


  private val fMap: String => Int = (s: String) => s.length
  println(
    s"""Monad: map function:
       |${s.map(fMap)}
       | same As
       |${s.mapWithFlatMap_Unit(fMap)}
       | same As
       |${s.mapWithFlatMap_Unit2(fMap)}
    """.stripMargin
  )

  //Associativity Law
  //obj flatMap f1 flatMap g1 == obj flatmap(x => f1(x) flatMap g1)
  val f1: String => Try_[Int] = (s: String) => Success_(s.length)
  val f2: Int => Try_[String] = (i: Int) => Success_(s"Length: $i")
  val val1 = s.flatMap(f1).flatMap(f2)
  val val2 = s.flatMap(x => f1(x) flatMap (f2))
  println(s"Monad Associativity Law: $val1 == $val2")

  //Right Unit Law
  //obj flatMap unit == obj
  val valRightUnit1 = s.flatMap(x => f1(x).unit(x))
  val valRightUnit2 = s
  println(s"Monad RightUnit Law: Type-Success: $valRightUnit1 == $valRightUnit2")

  val objFailure = Try_({
    throw new Exception("random exception")
  })
  val valRightUnitF1 = objFailure.flatMap(x => f3(x).unit(x))
  val valRightUnitF2 = objFailure
  println(s"Monad RightUnit Law: Type-Failure: $valRightUnitF1 == $valRightUnitF2")


  //Left Unit Law
  //unit(val1) flatMap(f) == f(val1)
  val valLeftUnit1 = Try_(15).flatMap(f2)
  val valLeftUnit2 = f2(15)
  println(s"Monad LeftUnit Law: Type-Success: $valLeftUnit1 == $valLeftUnit2")

  val f3: Exception => Try_[ArithmeticException] = e => Failure_(new ArithmeticException("converted"))
  val valLeftUnitF1 = Try_({
    throw new Exception("random exception")
  }).flatMap(f3)
  val valLeftUnitF2 = f3({
    throw new Exception("random exception")
  })
  println(s"Monad LeftUnit Law: Type-Failure:$valLeftUnitF1 == $valLeftUnitF2")
  //HERE, left unit law fails for Failure_ Type
  // as left-side is not throwing error while, right-side throws exception.
}
