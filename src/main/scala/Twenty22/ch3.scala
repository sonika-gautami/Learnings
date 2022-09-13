package Twenty22

object ch3 extends App {

  def insertionSort(a: Array[Int]): Array[Int] = {  //3rd run right
    var i = 1
    while (i < a.length) {

      var j = i
      while (j > 0 && a(j - 1) > a(j)) {
        val t = a(j - 1)
        a(j - 1) = a(j)
        a(j) = t

        j = j - 1
      }

      i = i + 1
    }
    a
  }

  println(insertionSort(Array(3, 5, 6, 7, 1, 8, 2, 4)).toList)
}
