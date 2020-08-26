package clrs_book

object MergeSort extends App {
  /*
  * Merge Sort:
  *
  * Divide Array in 2 pieces : until 0 element
  * Combine them in sorted : untill all
  *
  * 145632
  * 145 632
  * 1 45 6 32
  * 1 4 5 6 3 2
  * 1 45 6 23
  * 145 236
  * 123456
  * */


  //DIVIDE of Divide & Conquer [Divide into small sub-problems]
  def divide(arr: Array[Int]): (Array[Int], Array[Int]) = {
    val n = arr.length

    val d = (n / 2)
    (arr.take(d), arr.drop(d))
  }

  //CONQUER of Divide & Conquer [Solve sub-problem recursively]
  def mergeWithSorted(l: Array[Int], r: Array[Int]): Array[Int] = {
    //println(s"${l.toIterable}, ${r.toIterable}")
    if (l.length == 0) {
      r
    } else if (0 == r.length) {
      l
    } else if (l(0) < r(0)) {
      Array(l(0)) ++: mergeWithSorted(l.tail, r)
    } else {
      Array(r(0)) ++: mergeWithSorted(l, r.tail)
    }
  }


  def mergeWithSorted_NonRecursive(l: Array[Int], r: Array[Int]): Array[Int] = {
    var li = 0
    var ri = 0
    var newArr = new Array[Int](l.length + r.length)


    for (newI <- 0 until (l.length + r.length)) {
      if (li == l.length) {
        newArr(newI) = r(ri)
        ri = ri + 1
      } else if (ri == r.length) {
        newArr(newI) = l(li)
        li = li + 1
      } else if (l(li) < r(ri)) {
        newArr(newI) = l(li)
        li = li + 1
      } else {
        newArr(newI) = r(ri)
        ri = ri + 1
      }
    }

    println(s"Output: ${
      newArr.toIterable
    }")
    newArr
  }


  /*
  * 1st we're continuously dividing until size 1
  * Then, in reverse order merging them.
  *
  * The pattern of applying F1 in oder o1 &
  *  then applying F2 (on results) in reverse order of o1
  * That's where Recursion is.
  * */
  //COMBINE of Divide 7 Conquer [Combine sub-problem solutions into original problem]
  def applyMergeSort(arr: Array[Int]): Array[Int] = {
    if (arr.length > 1) {
      val (l, r) = divide(arr)
      mergeWithSorted(applyMergeSort(l), applyMergeSort(r)) //merge
    } else {
      arr
    }
  }
{

  val a = applyMergeSort(Array(2, 9, 8, 7, 5, 6, 1, 0, 3, 4))
  println(s"Output: ${
    a.toIterable
  }")

  val b = applyMergeSort(Array(2, 9, 8, 7, 5, 6, 1, 3, 4))
  println(s"Output: ${
    b.toIterable
  }")

  b.dropRight()

}

}
