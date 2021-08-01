package utils

object Timer {
  def now = System.currentTimeMillis()

  def timeOf(f: () => Unit): Unit = {
    val l = now
    f()
    println(s"Time taken: ${now - l} ms")
  }

}
