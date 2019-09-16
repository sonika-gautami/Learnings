package coursera.functional.design.recap

object collections extends App {

  /*
  Iterable:
    Seq:
      IndexedSeq:
        Vector, ..Array, ..String
      LinearSeq:
        List

    Set:

    Map:

   */

  /*
  abstract class testList[-A] {
    def map[B](f: A => B): testList[B] = this match {
      case Nil => Nil
      case h :: t => f(h) :: t.map(f)
    }

    def flatMap(f: A => List[B]) = this match {
      case Nil => Nil
      case h :: t => f(h) ::: t.flatMap(f)
    }

    def filter(f: A => Boolean) = this match {
      case Nil => Nil
      case h :: t => if(f(h)) h :: t.filter(f) else t.filter(f)
    }
  */
}
