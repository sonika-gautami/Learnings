//Ordering Trait

def sort[A](l: List[A])(ordering: Ordering[A]) =
  ordering.lt(l.head, l.tail.head)

sort(List(1, 2, 3))(Ordering.Int)

//In Scala,
// it is possible to use the same symbol for both types and values.
//i.e
//val Ordering : scala.math.Ordering.type
//object Ordering { ... }

//Passing the ordering argument is not just verbose (it is not very verbose, actually),
// the problem is that the ordering argument can be systematically inferred from the type of the elements to sort.
// Because it is so systematic, could it be automated instead?
//implicit params
//The compiler infers the argument value based on its expected type.

//A method can have ONLY ONE implicit parameter-LIST, and it must be the LAST parameter list given.

/*The compiler searches for definitions that:
    have type T,
    are marked implicit,
    are visible at the point of the function call, or are defined in a COMPANION OBJECT associated with T.
 */

//Any val, lazy val, def, or object definition can be marked implicit.

// implicit definitions can take type parameters and implicit PARAMETERS

//for implicit, first scope(import/defined), if not found then, it looks for Companion object of that type-T


trait Foo[A]

trait Bar[A] extends Foo[A]

trait Baz[A] extends Bar[A]

trait X

trait Y extends X

/*If an implicit value of type Bar[Y] is required, the compiler will look for implicit definitions in the following companion objects:

Bar, because it is a part of Bar[Y],
Y, because it is a part of Bar[Y],
Foo, because it is a parent type of Bar,
and X, because it is a parent type of Y.
*/

//if more than one implicit definition are eligible, an ambiguity is reported
//Actually, several implicit definitions matching the same type don’t generate an ambiguity if one is MORE SPECIFIC than the other.


trait A {
  implicit val x: Int = 0
}

trait B extends A {
  implicit val y: Int = 1

  def f(implicit n: Int) = ()

  f
}

//y is defined in a trait that extends A (which is where x is defined), y is more specific than x. Thus, there is no ambiguity and the compiler selects y.


//query an implicit value of a given type
implicitly[Ordering[Int]]


//Context Bounds:
//Syntactic sugar allows the omission of the implicit parameter list

//A: Ordering is Context Bound; below & is same as 2nd definition
def printSorted[A: Ordering](as: List[A]): Unit = println(sort(as))
def printSorted[A](as: List[A])(implicit ordering: Ordering[A]): Unit = println(sort(as))

//In General:
//def f[A: U ... : Un](ps): R = {} is same as
//def f[A](ps)(implicit p1 U[A], ..., pn Un[A])