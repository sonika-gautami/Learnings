package Twenty22

object Ruler extends App {

  ruler(1, 5)
  ruler(3, 3)

  def ruler(no: Int, len: Int): Unit = {
    def printDase(l: Int, noIfAny: String = ""): Unit = {
      0 until l foreach (_ => print("-"))
      println(noIfAny)
    }

    def numbers: Unit = {
      0 until no foreach { i =>
        printDase(len, i.toString)
        draw(len - 1)
      }
      printDase(len, no.toString)
    }

    def draw(l: Int): Unit = {
      if (l >= 1) {
        draw(l - 1)
        printDase(l)
        draw(l - 1)
      }
    }

    numbers
  }

}
