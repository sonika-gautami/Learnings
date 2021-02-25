package clrs_book

import utils.ConsolePrinter

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
  */


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


  /* 1st we're continuously dividing until size 1
  * Then, in reverse order merging them.
  *
  * The pattern of applying F1 in oder o1 &
  *  then applying F2 (on results) in reverse order of o1
  * That's where Recursion is.
  */
  //COMBINE of Divide 7 Conquer [Combine sub-problem solutions into original problem]
  def applyMergeSort(arr: Array[Int]): Array[Int] = {
    if (arr.length > 1) {
      val (l, r) = divide(arr)
      mergeWithSorted(applyMergeSort(l), applyMergeSort(r)) //merge
    } else {
      arr
    }
  }


  /*{

    val a = applyMergeSort(Array(2, 9, 8, 7, 5, 6, 1, 0, 3, 4))
    println(s"Output: ${
      a.toIterable
    }")

    val b = mergeSort1(Array(2, 9, 8, 7, 5, 6, 1, 3, 4))
    println(s"Output: ${
      b.toIterable
    }")

  }*/


  def mergeSort1(arr: Array[Int]): Array[Int] = {
    val n = arr.length

    if (n > 1) combine(arr.take(n / 2), arr.drop(n / 2))
    else arr
  }

  /*def divide(arr: Array[Int]): {
    if (n > 1) (arr.take (n / 2), arr.drop (n / 2) )
    else arr
  }*/

  def combine(l: Array[Int], r: Array[Int]): Array[Int] = {
    var li = 0
    var ri = 0

    val sorted = for (j <- 0 until (l.length + r.length))
      yield
        if (l(li) < r(ri)) {
          println(li)
          li = li + 1
          l(li - 1)
        } else {
          println(ri)
          ri = ri + 1
          r(ri - 1)
        }

    sorted.toArray
  }


  /*
  Given Array a ->
   divide in 2 until size = 1 ->
    combine in sorted order ->
   */

  def mergeSort(a: Array[Int]): Array[Int] = { //called logN times with N/level+1-no elements
    val n = a.length //1
    if (n > 1) { //1
      println(s"Inside: $n")
      val l = mergeSort(a.take(n / 2)) //1
      println(s"Left Done for ${l.size}")
      val r = mergeSort(a.drop(n / 2)) //1
      println(s"Right Done for ${r.size}")
      println(s"Combine: ${l.length}, ${r.length}")
      combine_(l, r) // m + n ~ 2n times
    }
    else {
      println("Base Condition")
      a //1
    }
  }

  def combine_(l: Array[Int], r: Array[Int]): Array[Int] = {
    var i = 0 //1
    var j = 0 //1

    val a = for (k <- 0 until (l.length + r.length)) // m + n ~ 2n times
      yield
        if (i >= l.length) { //1
          j = j + 1 //1
          r(j - 1) //1
        }
        else if (j >= r.length) { //1
          i = i + 1 //1
          l(i - 1) //1
        }
        else if (l(i) < r(j)) { //1
          i = i + 1 //1
          l(i - 1) //1
        } else { //1
          j = j + 1 //1
          r(j - 1) //1
        }

    a.toArray //1
  }


  /*
  Merge Sort -> For levaes apply insertion sort

  n = 1 => 1
  n = 2 => 2 + n(1)
  n = 3 => 3 + n(2)
  n = 4 => 4 + n(3)

  n                  2^0 root level
  n/2  no-of-lists = 2^1
  n/4                2^2
  n/8                2^3

//Insertion time
length = k => n=k -> theta(k2)
n/k lists of length-k = n/k* theta(k2) => theta(n/k * (k^2)) => theta(nk)

//merge sort
n * log(n/k)


nk + nlogN/K = n logN => k =?

take k =logN
nLogN + n (logN - log(logN))
nLogN + nlogN - nlog(logN)
2nLogN -nlog(logN)
~nLogN
   */

  def insertionSort11(a: Array[Int]) = {
    for (i <- 0 until a.length - 1) yield {
      val key = a(i + 1)
      var j = i
      while (j >= 0 && key < a(j)) {
        a(j + 1) = a(j)
        j = j - 1
      }
      a(j + 1) = key
    }
    a
  }


  def mergeWithInsertionSortOutCall(b: Array[Int]): Array[Int] = {

    val k = Math.floor(Math.log10(b.length) / Math.log10(2))
    println(s"---Choosing k = $k")

    def mergeWithInsertionSort(a: Array[Int]): Array[Int] = {
      if (a.length <= (if (k < 1) 1 else k)) {
        println(s"applying InsertSort-- ${a.length}")
        insertionSort11(a)
      } else {
        val n = a.length / 2
        val l = mergeWithInsertionSort(a.take(n))
        val r = mergeWithInsertionSort(a.drop(n))
        combine_(l, r)
      }
    }

    mergeWithInsertionSort(b)
  }


  //Invokes
  ConsolePrinter.invokePrintAssertForArray(mergeSort)("mergeSort")

  ConsolePrinter.invokePrintAssertForArray(mergeWithInsertionSortOutCall)("mergeWithInsertionSort")
}
