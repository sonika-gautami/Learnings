package test

object ClimbingStairs extends App {

  climbStairs1(3)
  // Either 1 or 2 steps - distinct ways can you climb to the top
  // fibonacci
  def climbStairs(n: Int): Int = {
    if (n == 1) 1
    else if (n == 2) 2
    else {
      var currMinus2 = 1
      var currMinus1 = 2
      3 to n foreach { _ =>
        val temp = currMinus1 + currMinus2
        currMinus2 = currMinus1
        currMinus1 = temp
      }
      currMinus1
    }
  }

  def climbStairs1(n: Int): Int = {
    val cache: Array[Int] = if (n < 3) null else new Array[Int](n - 2)

    def loop(i: Int): Int = {
      if (i == 1) 1
      else if (i == 2) 2
      else if (cache(i - 2 - 1) != 0) cache(i - 2 - 1)
      else {
        cache(i - 2 - 1) = loop(i - 1) + loop(i - 2)
        cache(i - 2 - 1)
      }
    }

    loop(n)
  }

}
