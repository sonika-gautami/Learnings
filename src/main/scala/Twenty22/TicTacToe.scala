package Twenty22

object TicTacToe extends App {

  class Self(var player: Int) {
    val EMPTY = 0
    val X = 1
    val Y = -1

    val positions = Array(
      Array(0, 0, 0),
      Array(0, 0, 0),
      Array(0, 0, 0))

    def isWin(marker: Int): Boolean =
      (0 until 3).exists(i =>
        (positions(i)(0) + positions(i)(1) + positions(i)(2)) == 3 * marker) ||
        (0 until 3).exists(i =>
          (positions(0)(i) + positions(1)(i) + positions(2)(i)) == 3 * marker) ||
        List(0, 2).exists(i =>
          (positions(0)(i) + positions(1)(1) + positions(2)(2 - i)) == 3 * marker)

    def winner(): Int = if (isWin(X)) X else if (isWin(Y)) Y else EMPTY

    override def toString: String = {
      "Board:\n" +
        (0 until (3)).map(i =>
          "|" + (0 until (3)).map(j => positions(i)(j)).mkString("|") + "|"
        ).mkString("\n")
    }

    def clearBoard: Unit = {
      for {i <- 0 until 3; j <- 0 until 3} positions(i)(j) = EMPTY
      player = X
    }

    def putMark(i: Int, j: Int) = {
      if (i < 0 || j < 0 || i > 2 || j > 2) throw new Error("Input out of Board")
      positions(i)(j) = player
      player = player * -1
    }
  }


  val game = new Self(-1);
  println(game)
  println(game.winner())

  game.putMark(0, 1);
  game.putMark(1, 1);
  game.putMark(1, 2);
  game.putMark(2, 2);
  game.putMark(2, 1);
  game.putMark(0, 0);
  println(game)
  println(game.winner())

  game.clearBoard
  println(game)
}
