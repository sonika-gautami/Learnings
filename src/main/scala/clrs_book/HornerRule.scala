package clrs_book

import utils.ConsolePrinter

object HornerRule extends App {

  /*
Polynomial:

f(x)= ∑i=0 until n  ai * x^i
OR
f(x) = a0 + a1*x + a2*x^2 + ... + an*x^n

HornerRule => algorithm used to simplify the process of evaluating a polynomial

f(x) = a0 + x(a1 + x(a2 + x(a3 + ... + x(an-1 + anx)....)

Another way to represent the results using this algorithm is through the tableau given below:
When n=5
	b5 = a5
	b4 = a4 + x*b5
	b3 = a3 + x*b4
	b2 = a2 + x*b3
	b1 = a1 + x*b2
	b0 = a0 + x*b1

The advantage offered by Horner's rule is that
 it reduces the number of multiplication operations; Hence, execution-time
   */

  //Considering a as co-efficient & index as x-term (x^index)
  /*
  n + ∑n
  n + n(n+1)/2
  n + n2/2 + n/2
  ~ n^2
  theta-N^2
   */
  def defaultF(x: Int = 2)(a: Seq[Int]): Int = { //theta-N2
    var sum = 0

    for (i <- 0 until a.length) { //n
      //Math.pow(x, i).toInt
      var m = 1
      for (j <- 0 to i) { //1,2,..n = ∑n = n(n+1)/2
        m = m * x
      }
      sum = sum + a(i) * m
    }
    sum
  }

  def hornerF(a: Seq[Int], x: Int = 2): Int = {
    var sum = 0
    for (i <- a.length - 1 to(0, -1)) {
      sum = if (x == 0) sum + a(i) else (sum + a(i)) * x
    }
    sum
  }

  ConsolePrinter.invokePrintAssert(
    f = defaultF(x = 2),
    a = Seq(1, 2, 3, 4),
    expected = hornerF(Seq(1, 2, 3, 4))
  )("HornerRule")
}
