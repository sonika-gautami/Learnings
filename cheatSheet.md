Type Parameters:

It is possible to restrict the type being used, e.g.

def myFct[T <: TopLevel](arg: T): T = { ... } // T must derive from TopLevel or be TopLevel
def myFct[T >: Level1](arg: T): T = { ... }   // T must be a supertype of Level1
def myFct[T >: Level1 <: Top Level](arg: T): T = { ... }


Variance:

Given A <: B

If C[A] <: C[B], C is covariant

If C[A] >: C[B], C is contravariant

Otherwise C is nonvariant


class C[+A] { ... } // C is covariant
class C[-A] { ... } // C is contravariant
class C[A]  { ... } // C is nonvariant



For a function,
 if A2 <: A1 and B1 <: B2, then A1 => B1 <: A2 => B2.



Functions must be
    contravariant in their argument types and
    covariant in their result types,
e.g.

trait Function1[-T, +U] {
  def apply(x: T): U
} // Variance check is OK because T is contravariant and U is covariant

class Array[+T] {
  def update(x: T)
} // variance checks fails




val nums = Vector("louis", "frank", "hiromi")
nums(1)                     // element at index 1, returns "frank", complexity O(log(n))
nums.updated(2, "helena")   // new vector with a different string at index 2, complexity O(log(n))



Ordering:

There is already a class in the standard library that represents orderings:
 scala.math.Ordering[T]
 which contains comparison functions such as lt() and gt() for standard types. 
Types with a single natural ordering should inherit from the trait scala.math.Ordered[T].

import math.Ordering  
def msort[T](xs: List[T])(implicit ord: Ordering) = { ...}  
msort(fruits)(Ordering.String)  
msort(fruits)   // the compiler figures out the right ordering  


