package utils

object ConsolePrinter {

  def invokePrintAssertForArray(f: Array[Int] => Array[Int],
                                a: Array[Int] = Array(2, 9, 8, 7, 5, 6, 1, 0, 3, 4),
                                expected: Array[Int] = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
                               )
                               (nameOfFunction: String): Unit = {

    println(s"----------Applying: $nameOfFunction------${f.getClass}-------------")
    println(s"Input: ${a.toIterable}")

    val result = f(a)

    println(s"Output: ${result.toIterable}")
    println(s"-------------------------------------------------------------------")

    assert(result.toSeq == expected.toSeq)
  }


  def invokePrintAssert[T, RES](f: T => RES,
                                a: T,
                                expected: RES
                               )
                               (nameOfFunction: String): Unit = {

    println(s"----------Applying: $nameOfFunction------${f.getClass}-------------")
    println(s"Input: $a")

    val result = f(a)

    println(s"Output: ${result}")
    println(s"-------------------------------------------------------------------")

    assert(result == expected)
  }


  def invokePrint[T, RES](f: T => RES,
                          a: T
                         )
                         (nameOfFunction: String): Unit = {

    println(s"----------Applying: $nameOfFunction------${f.getClass}-------------")
    println(s"Input: $a")

    val result = f(a)

    println(s"Output: ${result}")
    println(s"Input after applying Function: $a")
    println(s"-------------------------------------------------------------------")
  }

}
