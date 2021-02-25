package clrs_book

object InsertionSort extends App {

  /*

  8 n^2 > 64 n log-n
  n > 8 log-n
  n/8 > log-n

    As, log n of base b = x =>  n = b^x

  2^ n/8 > n

Lets say
n  = 8 -> 2 > 8
n = 16 -> 4 < 16
n = 24 -> 8 < 24
n = 32 -> 16 < 32
n = 40 -> 32 < 40
n = 48 -> 64 < 48     <-- Crossed  Between 40 & 48
n = 56 -> 128 < 56


100 n^2  < 2^n

Lets say
n = 1 100 < 2
n = 7 100 * 49 < 128
n = 50 100* 2500 < 2^10 * 5


   */

  {//Insertion Sort
    val a = Array(5, 7, 6, 1, 8, 3, 10, 2, 0, 4, 9)
    println(s"Input:  ${a.toIterable}")

    for (j <- 1 until a.length) { //n-1 + 1(false condition)// = n
      val key = a(j) //n-1
    var i = j - 1 //n-1

      while (i >= 0 && a(i) > key) {
        /*1 + 1(false condition),
                                              2 + 1(false condition),
                                               .. ,
                                              n-1 + 1(false condition)
                                            => E(i=2 to n)N*/
        a(i + 1) = a(i) // E(i=1 to n-1)N-1
        i = i - 1 //  E(i=1 to n-1)N-1
      }

      a(i + 1) = key //n-1
    }
    println(s"Output: ${a.toIterable}")
  }

  {//Insertion Sort NA
    import scala.util.control.Breaks._

    val a = Array(5, 7, 6, 1, 8, 3, 10, 2, 0, 4, 9)
    for (j <- 1 until a.length) {
      breakable {
        for (i <- (j - 1) to(0, -1)) {
          //println(s"i: $i")

          if (a(i) < a(i + 1)) break;

          val key = a(i + 1)
          a(i + 1) = a(i)
          a(i) = key
          //println(s"output: ${a.toIterable}")
        }
      }
    }

    println(s"Output: ${a.toIterable}")


    //Linear Search:
    //Worst Case -> element is at last -> n times
    //Best Case -> element is at first -> Constant Time
    //Avg Case -> n/2 -> ~ n times
    val valueToLookFor = 10
    var index: Option[Int] = None
    breakable {
      for (i <- 0 until a.length) {
        if (a(i) == valueToLookFor) {
          index = Some(i)
          break()
        }
      }
    }
    println(s"$valueToLookFor avialble at: ${index}")
  }


  /* InsertionSort:
9154732
915473
91547
9154
915
91
9
19 9X 99 19
159 19X 199 159
1459  159X 1599 1559 1459
14579 1459X 14599 14579
134579 14579X 145799 145779 145579 144579 134579
1234579 134579X 1345799 1345779 1345579 1344579 1334579 1234579
   */
  /*
  n times called:
  n=1 -> 1
  n=2 -> (n=1)1 + (n=2)1
  n=3 -> (n=1)1 + (n=2)1 + (n=3)2
    ..
  n*n-1
  ~ n^2
   */
  def insertionSortRec(a: Array[Int]): Array[Int] = {   //called -> n times

    if (a.length > 1) {    //1
      val key = a(a.length -1)  //1

      println(s"Inside: ${a.toIterable}")
      //(Int.MaxValue) PlaceHolder For key to insert
      val b: Array[Int] = insertionSortRec(a.dropRight(1)).:+(Int.MaxValue) //1
      println(s"Inside21: ${b.toIterable}")

      var i = b.length - 1 - 1        //1
      while (i >= 0 && b(i) > key) {  // n-1
        b(i + 1) = b(i)   //n-1
        i = i - 1         //n-1
        println(s"Inside22: ${b.toIterable}")
      }
      b(i + 1) = key //1

      println(s"Inside2: ${b.toIterable}")
      b
    }
    else a  //1
  }

  val a = Array(5, 7, 6, 1, 8, 3, 10, 2, 0, 4, 9)
  println(s"Input:  ${a.toIterable}")
  println(s"O/p of insertionSortRec: ${insertionSortRec(a).toIterable}.")


  def insertionSort_InsertingKeyWithBinarySearchInsteadLinear(): Unit = {
    //Binary Serach can be used, but due to shifting of nos, we can't reduce time of Algo running.
  }
}
