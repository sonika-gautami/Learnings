package clrs_book

import utils.ConsolePrinter

object BubbleSort extends App {

  def bubbleSort(a: Array[Int]): Array[Int] = {
    for {
      i <- 0 until a.length - 1
      j <- 0 until a.length - 1 - i
    } {

      if (a(j) > a(j + 1)) {
        val t = a(j)
        a(j) = a(j + 1)
        a(j + 1) = t
      }
    }
    a
  }

  //Invoke
  ConsolePrinter.invokePrintAssertForArray(bubbleSort)("bubbleSort")


  /*
  Loop Variant:
   3 necessary properties should be hold for the loop invariant:

  Initialization:

  Maintenance:

  Termination:
   */
}
