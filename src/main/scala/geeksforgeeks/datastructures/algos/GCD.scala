package geeksforgeeks.datastructures.algos

//GCD / HCF
object GCD extends App {

  //Euclidean algorithm:
  // 3, 6
  /*
  6 - 3, 3
  3, 3 -> 3
   */
  def gcd(a: Int, b: Int): Int = {
    if (a == 0) b
    else if (b == 0) a
    else if (a == b) a
    else if (a > b) gcd(a - b, b)
    else gcd(b - a, a)
  }

  //Euclidean algorithm: with modulo [efficient than minus one]
  // O(Log min(a, b))
  /*
  3 6
  6 %3, 3 -> 0, 3

   */
  def gcd2(a: Int, b: Int): Int = {
    if (a == 0) b
    else if (b == 0) a
    else if (a > b) gcd(a % b, b)
    else gcd(b % a, a)
  }

  //Extended Euclidean Algorithm
  /*
  ax + by = gcd(a, b)
  until a or b == 0

  Initial values -> x1,x2 -> 1,0  &  y1,y2 -> 0,1
  a & b ->  if(a > b) a else b

  next b = b % a & b
  next x1 = x2, x2 = x1 - x2 *(a/b)
  next y1 = y2, y2 = y1 - y2 *(a/b)

  Use:
  when a and b are coprime (or gcd is 1).

   */
  type gcd_X_Y = (Int, Int, Int)

  def gcd3(a: Int, b: Int): gcd_X_Y = {

    def loop(a: Int, b: Int, x1: Int, x2: Int, y1: Int, y2: Int): gcd_X_Y = {
      if (b == 0) (a, x1, y1)
      else loop(
        b, a % b,
        x2, x1 - x2 * (a / b),
        y2, y1 - y2 * (a / b)
      )
    }

    loop(if (a > b) a else b, if (a > b) b else a,
      1, 0,
      0, 1)
  }


  def gcdWithFactors(a: Int, b: Int): Int = {
    val primes = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 47)

    def loop(no: Int, pointer: Int = 0): List[Int] = {
      if (no == 1) List(no)
      else if (no % primes(pointer) == 0) (loop(no / primes(pointer)) :+ primes(pointer))
      else loop(no, pointer + 1)
    }

    val fa = loop(a)
    val fb = loop(b)

    fa.filter(fb.contains(_)).reduce(_ * _)
  }

  println("gcd: " + gcd(15, 12))
  println("gcd: " + gcd(150, 19))

  println("gcd2: " + gcd2(15, 12))
  println("gcd2: " + gcd2(150, 19))

  println("gcd3: " + gcd3(15, 12))
  println("gcd3: " + gcd3(150, 19))

  println("gcdWithFactors: " + gcdWithFactors(15, 12))
  println("gcdWithFactors: " + gcdWithFactors(150, 19))
}
