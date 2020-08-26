package clrs_book

object SelectionSort extends App {

  val a = Array(5, 7, 6, 1, 8, 3, 10, 2, 0, 4, 9)
  println(s"Input:  ${a.toIterable}")

  //Worst case > Avg Case -> Best Case = Same results = Θ(n2)
  for (i <- 0 until (a.length - 1)) { //c1 * n-1

    var indexOfSmaller = i //c2
    for (j <- i until a.length) { // c3 * ∑(i until n)
      if (a(indexOfSmaller) > a(j)) { //c4
        indexOfSmaller = j //c5
      }
    }

    val t = a(i) //c6
    a(i) = a(indexOfSmaller) //c7
    a(indexOfSmaller) = t //c8
  }

  println(s"Output: ${a.toIterable}")
}
