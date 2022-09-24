package test

object NQueues extends App {

  println(solveNQueens(1))
  println(solveNQueens(3))
  println(solveNQueens(4))
  println(solveNQueens(5))


  def solveNQueens(n: Int): List[List[String]] = {
    import scala.collection.mutable.ListBuffer
    val solutions = new ListBuffer[List[String]]()

    val board = new Array[Array[Char]](n)
    for (i <- 0 until n) board(i) = new Array[Char](n)
    for {
      i <- 0 until n
      j <- 0 until n
    } board(i)(j) = '.'

    def appendSolution = solutions.append(board.map(a => a.mkString("")).toList)

    def isSafe(r: Int, c: Int): Boolean = {
      (0 until n).foreach { i =>
        if (board(r)(i) == 'Q') return false
      }
      (0 until n).foreach { i =>
        if (board(i)(c) == 'Q') return false
      }

      var i = r
      var j = c
      while (i >= 0 && j >= 0) {
        if (board(i)(j) == 'Q') return false
        i = i - 1
        j = j - 1
      }

      i = r
      j = c
      while (i < n && j >= 0) {
        if (board(i)(j) == 'Q') return false
        i = i + 1
        j = j - 1
      }

      i = r
      j = c
      while (i >= 0 && j < n) {
        if (board(i)(j) == 'Q') return false
        i = i - 1
        j = j + 1
      }

      i = r
      j = c
      while (i < n && j < n) {
        if (board(i)(j) == 'Q') return false
        i = i + 1
        j = j + 1
      }

      true
    }

    def nqueensCol(r: Int): Unit = {
      if (r == n) appendSolution
      else {
        (0 until n).foreach { c =>
          if (isSafe(r, c)) {
            board(r)(c) = 'Q'
            nqueensCol(r + 1)

            //back-tracking
            board(r)(c) = '.'
          }
        }
      }
    }

    nqueensCol(0)
    solutions.toList.map(_.toList)
  }

}
