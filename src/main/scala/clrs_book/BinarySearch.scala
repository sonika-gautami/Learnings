package clrs_book

object BinarySearch extends App {

  def binarySearch(a: Array[Int], ele: Int, acc: Int = 0): Int = {
    val mid = a.length / 2

    println(s"mid: ${mid} acc: $acc")

    if (a(mid) == ele) acc + mid
    else if (a.length == 1) -1
    else if (ele < a(mid)) binarySearch(a.take(mid), ele, acc)
    else binarySearch(a.drop(mid), ele, acc + mid)
  }


  {

    /*
    7
    5678 mid=4
    78   mid=2
    7    mid

    4
    1234
    34   mid=2
    4    mid=3

    9
    5678 mid=4
    78   mid=2
    8    mid=1
     */
    val a = Array(1, 2, 3, 4, 5, 6, 7, 8)
    assert(binarySearch(a, 4) == 3)
    println("----")
    assert(binarySearch(a, 7) == 6)
    println("----")
    assert(binarySearch(a, 9) == -1)
  }


  /*
  ~ n*logN

   given a set S of n integers and another integer x,
    determines whether or not there exist two elements in S whose sum is exactly x.

    sort S with MergeSort -> n*logN
    x - nodeEle  < nodeEle -> go left else go right if zero then return (binarySerach logN)

    So, total is n*logN + n*logN => 2*n*logN => ~n*logN
   */

  def binarySearch_SumOf2ElesIsEqualX(a: Array[Int], x: Int = 0): Boolean = {
    a.find(n => binarySearch(a, x - n) != -1).isDefined
  }

  {
    val a = Array(1, 2, 3, 4, 5, 6, 7, 8)
    assert(binarySearch_SumOf2ElesIsEqualX(a, 9))
    assert(binarySearch_SumOf2ElesIsEqualX(a, 16))
  }

}
