package geeksforgeeks.datastructures.algos

//Prime Numbers
object Prime extends App {

  //To find primes: Sieve of Eratosthenes //coursera
  def primes(n: Int): Seq[Int] = {
    def sieve(l: Stream[Int]): Stream[Int] = l.head #:: sieve(l.tail.filter(_ % l.head != 0))

    sieve(Stream.from(2)).take(n)
  }

  //Sieve of Eratosthenes
  //TimeComplexity: noOfPrimes*n  ~ o(n*C)
  //SpaceComplexity: o(n)
  def primesSelf(n: Int): List[Int] = {
    def newList(list: List[Int], multipleOf: Int) =
      list.filter(i => i % multipleOf != 0 || i == multipleOf)

    def loop(pointer: Int, l: List[Int]): List[Int] =
      if (pointer + 1 < l.size) loop(pointer + 1, newList(l, l(pointer))) else l

    loop(0, (2 to n).toList)
  }

  println("First 10 primes: " + primes(10).mkString(", "))
  println("First 10 primes: " + primesSelf(30).mkString(", "))
  println("First 300 primes: " + primes(200).mkString(", "))
}
