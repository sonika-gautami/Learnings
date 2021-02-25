package clrs_book

object InversionsInArray extends App {

  /*
  i < j  & a(i) > a(j) then pair(i, j) is Inversion Pair.
   */
  //theta-n^2
  def inversionsOf(a: Array[Int]): Seq[(Int, Int)] = {
    for {
      i <- 0 until a.length - 1 //n-1
      j <- i + 1 until a.length //n-1, n-2, .., 2,1
      if (a(i) > a(j))
    } yield {
      println(s"Indexes: $i < $j AND ${a(i)} > ${a(j)}")
      i -> j
    }
  }

  /*
  set {1,2,3, ..,n}

  Most Inversions -> How-many
  0 0 if sorted

  if reversed then Highest
    { n, n-1, ..., 3,2,1 }
    1 -> n-1 (1st index has n-1 inversions)
    2 -> n-2
    3 -> n-3
      ...
    n-1 -> n-n+1 = 1

    Taking sum of above (starting from down)
      = 1,2, .., n-2, n-1
      = ∑n - n [deducting n as n is not included]
      = n(n+1)/2 -n
      = n^2/2 + n/2 -n
      = n^2/2 - n/2 [Total Inversions in Array when set is reversed.]

      OR

     As every unique pair of ReverseOrdered Set is InversionPair
     => Taking 2 distinct values from n elements = nC2 = n(n-1)/2
   */

  /* Inversions By Merge-Sort
  23861
  2 3861
  3 861
  8 61
  6 1

  23861
  23 861 [2]
  2 3 8 61
  2 3 8 6 1
  23 8 6 1
  23 8 16 [1]
  23 168  [2]
  12368   [2]


  23 8 61 [1]
  23 861 [2]
  23816  [1 + 1]
   */
  //theta-nlogN
  def countOfInversionsOfByMerge(acc: Int = 0)(a: Array[Int]): Int = {
    val l = a.length
    if (l > 1) {
      val le = a.take(l / 2)
      val ri = a.drop(l / 2)
      println(s"Left: ${le.toIterable}")
      println(s"Left: ${ri.toIterable}")

      var i = 0
      var j = 0
      val c1 = countOfInversionsOfByMerge(acc)(le) +
        countOfInversionsOfByMerge(acc + l / 2)(ri)

      val c2 = for (k <- 0 until le.length + ri.length) //le + ri
        yield {
          if (i >= le.length) {
            a(k) = ri(j)
            j = j + 1
            0
          } else if (j >= ri.length) {
            a(k) = le(i)
            i = i + 1
            0
          } else if (le(i) > ri(j)) {
            a(k) = ri(j)
            j = j + 1
            le.length - (i + 1) + 1
          } else {
            a(k) = le(i)
            i = i + 1
            0
          }
        }

      println(s"Array: ${a.toIterable}, count: ${c2.sum}")
      c1 + c2.sum
    }
    else 0
  }


  //Invoke
  utils.ConsolePrinter.invokePrint(
    inversionsOf, Array(2, 3, 8, 6, 1))("inversionsInArray")

  private val a2 = Array(2, 3, 8, 6, 1)
  utils.ConsolePrinter.invokePrint(
    f = countOfInversionsOfByMerge(acc = 0),
    a = a2)("countOfInversionsOfByMerge")
  println(a2.toIterable)

  utils.ConsolePrinter.invokePrint(
    f = countOfInversionsOfByMerge(acc = 0),
    a = Array(2, 3, 8, 6, 1, 0, 9, 7, 5))("countOfInversionsOfByMerge")

  utils.ConsolePrinter.invokePrint(
    inversionsOf, a = Array(2, 3, 8, 6, 1, 0, 9, 7, 5))("inversionsInArray")

}
