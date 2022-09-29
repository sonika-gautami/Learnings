package grokking

object Utils {

  def printAndAssert[T](f: () => T, expected: T): Unit = {
    val result = f()
    println(s"Answer = $result")
    assert(result == expected)
  }

  def print[T](f: () => T, expected: T): Unit = {
    val result = f()
    println(s"Answer (${result == expected}) = $result")
  }

}
