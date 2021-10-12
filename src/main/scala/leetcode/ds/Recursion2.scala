package leetcode.ds

import scala.collection.{immutable, mutable}

object Recursion2 extends App {

  {
    println(generateParenthesis(1))
    println(generateParenthesis(2))
    println(generateParenthesis(3))
    println(generateParenthesis(4))

    def generateParenthesis(n: Int): List[String] = {
      def loop(i: Int, acc: String): List[String] = {
        if (i == n) List("()" + acc)
        else
          loop(i + 1, ")" + acc).map(s => "(" + s) ++
            loop(i + 1, acc).map(s => "()" + s) ++ {
            (for (j <- 1 to acc.length) yield loop(i + 1, acc.drop(j)).map(s => "()" + acc.take(j) + s)).flatten
          }
      }

      loop(1, "")
    }

  }

  {
    var treeNode = TreeNode(6,
      TreeNode(21,
        TreeNode(1),
        TreeNode(4, TreeNode(-31), TreeNode(5))),
      TreeNode(7, right = TreeNode(9, left = TreeNode(8))))

    var treeNode2 = TreeNode(1,
      TreeNode(2, TreeNode(3), TreeNode(4)),
      TreeNode(2, TreeNode(4), TreeNode(3)))

    println(isSameTree(treeNode, treeNode2))
    println(isSameTree(treeNode, treeNode))
    println(isSameTree(treeNode2, treeNode2))

    def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
      if (p == null && q == null) return true
      if (p == null || q == null) return false


      import scala.collection.mutable.Stack
      val leftS = new Stack[TreeNode]()
      val rightS = new Stack[TreeNode]()
      leftS.push(p)
      rightS.push(q)

      while (leftS.nonEmpty) {
        val l = leftS.pop()
        val r = rightS.pop()

        if (l.value == r.value &&
          ((l.left != null && r.left != null) || (l.left == null && r.left == null)) &&
          ((l.right != null && r.right != null) || (l.right == null && r.right == null))) {

          if (l.left != null) {
            leftS.push(l.left)
            rightS.push(r.left)
          }
          if (l.right != null) {
            leftS.push(l.right)
            rightS.push(r.right)
          }

        } else {
          return false
        }
      }

      true
    }
  }

  {
    println("---------------")
    var sol: List[List[Int]] = Nil
    sol = combine_backtrack(n = 4, k = 2)
    println(sol)
    sol = combine_backtrack(n = 4, k = 3)
    println(sol)
    sol = combine_backtrack(n = 5, k = 3)
    println(sol)
    sol = combine_backtrack(n = 1, k = 1)
    println(sol)

    println("---------------")

    def combine_backtrack(n: Int, k: Int): List[List[Int]] = {
      import scala.collection.mutable.ListBuffer
      val sol: ListBuffer[List[Int]] = new ListBuffer[List[Int]]()

      def loop(cuurentK: Int, acc: List[Int], start: Int): Unit = {
        if (cuurentK > k) sol.append(acc)
        else if (start <= n) {
          loop(cuurentK + 1, acc :+ start, start + 1)
          //backtrack
          loop(cuurentK, acc, start + 1)
        }
      }

      loop(1, Nil, 1)
      sol.toList
    }

    //all possible combinations of k numbers out of the range [1, n].
    def combine(n: Int, k: Int): List[List[Int]] = {

      def remainings(start: Int, remaingsK: Int, acc: List[List[Int]]): List[List[Int]] = {
        if (remaingsK == 0) acc
        else {
          val max = if ((n - k) + start > n) n else (n - k) + start

          (start to max).foldLeft(Nil: List[List[Int]]) {
            (l: List[List[Int]], e: Int) =>
              val newAcc = acc.map(eles => eles :+ e)
              remainings(e + 1, remaingsK - 1, newAcc) ::: l
          }
        }
      }

      remainings(1, k, List(List[Int]()))
    }
  }
  {
    println("---------------")
    var board = Array(
      Array('5', '3', '.', '.', '7', '.', '.', '.', '.'),
      Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
      Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
      Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
      Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
      Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
      Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
      Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
      Array('.', '.', '.', '.', '8', '.', '.', '7', '9'))

    solveSudoku(board)
    println(board.map(_.toList).toList)
    assert(board.map(_.toList).toList ==
      Array(
        Array('5', '3', '4', '6', '7', '8', '9', '1', '2'),
        Array('6', '7', '2', '1', '9', '5', '3', '4', '8'),
        Array('1', '9', '8', '3', '4', '2', '5', '6', '7'),
        Array('8', '5', '9', '7', '6', '1', '4', '2', '3'),
        Array('4', '2', '6', '8', '5', '3', '7', '9', '1'),
        Array('7', '1', '3', '9', '2', '4', '8', '5', '6'),
        Array('9', '6', '1', '5', '3', '7', '2', '8', '4'),
        Array('2', '8', '7', '4', '1', '9', '6', '3', '5'),
        Array('3', '4', '5', '2', '8', '6', '1', '7', '9')).map(_.toList).toList
    )

    board = Array(Array('.', '.', '9', '7', '4', '8', '.', '.', '.'), Array('7', '.', '.', '.', '.', '.', '.', '.', '.'), Array('.', '2', '.', '1', '.', '9', '.', '.', '.'), Array('.', '.', '7', '.', '.', '.', '2', '4', '.'), Array('.', '6', '4', '.', '1', '.', '5', '9', '.'), Array('.', '9', '8', '.', '.', '.', '3', '.', '.'), Array('.', '.', '.', '8', '.', '3', '.', '2', '.'), Array('.', '.', '.', '.', '.', '.', '.', '.', '6'), Array('.', '.', '.', '2', '7', '5', '9', '.', '.'))
    solveSudoku(board)
    println(board.map(_.toList).toList)
    assert(board.map(_.toList).toList ==
      List(
        List('5', '1', '9', '7', '4', '8', '6', '3', '2'),
        List('7', '8', '3', '6', '5', '2', '4', '1', '9'), //'2''5'
        List('4', '2', '6', '1', '3', '9', '8', '7', '5'),
        List('3', '5', '7', '9', '8', '6', '2', '4', '1'),
        List('2', '6', '4', '3', '1', '7', '5', '9', '8'),
        List('1', '9', '8', '5', '2', '4', '3', '6', '7'), //'2''5'
        List('9', '7', '5', '8', '6', '3', '1', '2', '4'),
        List('8', '3', '2', '4', '9', '1', '7', '5', '6'),
        List('6', '4', '1', '2', '7', '5', '9', '8', '3')))
    println("---------------")


    def solveSudoku(board: Array[Array[Char]]): Unit = {

      val chars = List('1', '2', '3', '4', '5', '6', '7', '8', '9')

      def isRepeated(row: Int, col: Int, no: Char): Boolean = {
        def sameRow = 0 to (8) exists (i => board(i)(col) == no)

        def sameCol = 0 to (8) exists (j => board(row)(j) == no)

        def sameBoard = {
          val initialRow = (row / 3) * 3
          val initialCol = (col / 3) * 3

          initialRow until (initialRow + 3) exists { i =>
            initialCol until (initialCol + 3) exists (j => board(i)(j) == no)
          }
        }

        sameRow || sameCol || sameBoard
      }

      def putOnBoard(row: Int, col: Int): Boolean = {
        def nextItem = {
          if (row == 8 && col == 8) true
          else if (col == 8) putOnBoard(row + 1, 0)
          else putOnBoard(row, col + 1)
        }

        if (board(row)(col) != '.') nextItem
        else {
          var b = false
          chars.filter(_ => !b).foreach { n =>
            if (!isRepeated(row, col, n)) {
              board(row)(col) = n
              b = nextItem
            }
          }
          if (!b) {
            board(row)(col) = '.'
          }
          b
        }
      }

      putOnBoard(0, 0)
    }
  }


  println("---------------")
  println(totalNQueens(1))
  println(totalNQueens(2))
  println(totalNQueens(3))
  println(totalNQueens(4))
  println(totalNQueens(5))
  println(totalNQueens(6))
  println(totalNQueens(7))
  println(totalNQueens(8))
  println(totalNQueens(9))
  println("---------------")

  def totalNQueens(n: Int): Int = {
    val placedQueens = new mutable.Stack[Tuple2[Int, Int]]

    def inQueenCoveredZone(row: Int, col: Int): Boolean =
      placedQueens.exists {
        case (qi, qj) =>
          def rowOrCol = row == qi || col == qj

          def diagonals: Boolean = {
            def loopFor(i: Int, j: Int, iCounter: Int, jCounter: Int): Boolean = {
              if (i >= 0 && j >= 0 && i < n && j < n) {
                if (row == i && col == j) true
                else loopFor(i + iCounter, j + jCounter, iCounter, jCounter)
              } else false
            }

            loopFor(qi + 1, qj + 1, 1, 1) ||
              loopFor(qi + 1, qj - 1, 1, -1) ||
              loopFor(qi - 1, qj + 1, -1, 1) ||
              loopFor(qi - 1, qj - 1, -1, -1)
          }

          rowOrCol || diagonals
      }

    def putQueens(row: Int, col: Int): Unit = {
      if (row < n) {
        if (col >= n)
          putQueens(row + 1, 0)
        else if (!inQueenCoveredZone(row, col)) {
          //println(s"${row -> col}")
          placedQueens.push(row -> col)
          putQueens(row + 1, 0)
        } else {
          putQueens(row, col + 1)
        }
      }
    }

    (0 until n).map {
      i =>
        placedQueens.clear()
        placedQueens.push(0 -> i)
        //println(s"\nPlacedQueues: \n ${0 -> i}")
        var count = 0
        putQueens(row = 1, 0)
        if (n == placedQueens.length) count = count + 1

        while (placedQueens.length > 1) {
          //println(s"Poping: ${placedQueens.top}")
          val t2 = placedQueens.pop()
          putQueens(t2._1, t2._2 + 1)
          if (n == placedQueens.length) count = count + 1
        }
        count
    }.sum
  }

  var b = searchMatrix(
    matrix = Array(
      Array(1, 4, 7, 11, 15),
      Array(2, 5, 8, 12, 19),
      Array(3, 6, 9, 16, 22),
      Array(10, 13, 14, 17, 24),
      Array(18, 21, 23, 26, 30)),
    target = 5)
  println(b)

  b = searchMatrix(
    matrix = Array(
      Array(1, 4, 7, 11, 15),
      Array(2, 5, 8, 12, 19),
      Array(3, 6, 9, 16, 22),
      Array(10, 13, 14, 17, 24),
      Array(18, 21, 23, 26, 30)),
    target = 20)
  println(b)

  b = searchMatrix(
    matrix = Array(Array(-1, 3)), target = 3)
  println(b)

  b = searchMatrix(matrix = Array(
    Array(3, 5, 9, 9, 9, 11),
    Array(5, 8, 13, 13, 16, 17),
    Array(10, 10, 14, 14, 16, 19),
    Array(15, 18, 20, 24, 26, 26),
    Array(20, 24, 29, 32, 37, 41)), target = 32)
  println(b)

  //sorted in row & sorted in col
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    def loop(rowS: Int, colS: Int, rowE: Int, colE: Int): Boolean = {
      if (rowS == rowE && colS == colE) target == matrix(rowS)(colS)
      else {
        val midI = ((rowE - rowS) / 2) + rowS
        val midJ = ((colE - colS) / 2) + colS
        println(s"$rowS-$colS to $rowE-$colE : mid = $midI-$midJ :${
          matrix(midI)(midJ)
        }")

        if (target == matrix(midI)(midJ)) true
        else if (target > matrix(midI)(midJ)) {
          if (midJ + 1 <= colE && loop(rowS, midJ + 1, rowE, colE))
            true
          else
            midI + 1 <= rowE && loop(midI + 1, colS, rowE, midJ)
        } else {
          if (midI - 1 >= rowS && loop(rowS, colS, midI - 1, colE))
            true
          else
            midJ - 1 >= colS && loop(midI, colS, rowE, midJ - 1)
        }
      }
    }

    if (matrix.isEmpty) false
    else loop(0, 0, matrix.length - 1, matrix(0).length - 1)
  }


  var treeNode = TreeNode(6,
    TreeNode(21,
      TreeNode(1),
      TreeNode(4, TreeNode(-31), TreeNode(5))),
    TreeNode(7, right = TreeNode(9, left = TreeNode(8))))
  println(isValidBST(treeNode))

  var treeNode2 = TreeNode(1,
    TreeNode(2, TreeNode(3), TreeNode(4)),
    TreeNode(2, TreeNode(4), TreeNode(3)))
  println(isValidBST(treeNode2))

  var treeNode3 = TreeNode(5,
    TreeNode(2, TreeNode(1)),
    TreeNode(7, TreeNode(3), TreeNode(9)))
  println(isValidBST(treeNode3))

  var treeNode4 = TreeNode(5,
    TreeNode(2, TreeNode(1)),
    TreeNode(7, TreeNode(6), TreeNode(9)))
  println(isValidBST(treeNode4))


  def isValidBST(root: TreeNode): Boolean = {

    def loop(n: TreeNode, min: Option[Int], max: Option[Int]): Boolean = {
      def validate: Boolean =
        min.forall(n.value > _) && max.forall(n.value < _)

      if (n != null) {
        //Divide
        val left = loop(n.left, min, Some(n.value))
        val right = loop(n.right, Some(n.value), max)

        /*Conquer*/ validate && /*Combine*/ left && right
      }
      else true
    }

    loop(root, None, None)
  }

  def isValidBST_canBeImproved(root: TreeNode): Boolean = {

    def loop(n: TreeNode, accLeft: List[Int], accRight: List[Int]): Boolean = {
      def validate: Boolean =
        accLeft.forall(n.value < _) && accRight.forall(n.value > _)

      if (n != null) {
        //Divide
        val left = loop(n.left, accLeft :+ n.value, accRight)
        val right = loop(n.right, accLeft, accRight :+ n.value)

        /*Conquer*/ validate && /*Combine*/ left && right
      }
      else true
    }

    loop(root, Nil, Nil)
  }

  def isValidBST_NA(root: TreeNode): Boolean = {
    (root.left == null || root.left.value < root.value) &&
      (root.right == null || root.right.value > root.value) &&
      (root.left == null || isValidBST(root.left)) &&
      (root.right == null || isValidBST(root.right))
  }

  {
    var sol = Array[Int]()
    sol = sortArray(nums = Array(5, 2, 3, 1))
    println(sol.toList)

    sol = sortArray(nums = Array(5, 1, 1, 2, 0, 0))
    println(sol.toList)

    sol = sortArray(nums = Array(5, 1, 8, 9, 7, 6, 1, 2, 0, 0, 3, 4, 11))
    println(sol.toList)

    sol = sortArray(nums = Array(5))
    println(sol.toList)

    sol = sortArray(nums = Array())
    println(sol.toList)

    def sortArray(nums: Array[Int]): Array[Int] = {

      def sort(start: Int, level: Int): Int = {
        val mid = start + level
        if (mid >= nums.length) return nums.length

        val end = {
          val end = mid + level
          if (end >= nums.length) nums.length else end
        }

        val temp = new Array[Int](end - start)
        var index = 0
        var l = start
        var r = mid

        while (l < mid && r < end) {
          if (nums(l) < nums(r)) {
            temp(index) = nums(l)
            l = l + 1
          } else {
            temp(index) = nums(r)
            r = r + 1
          }
          index = index + 1
        }
        while (l < mid) {
          temp(index) = nums(l)
          l = l + 1
          index = index + 1
        }
        while (r < end) {
          temp(index) = nums(r)
          r = r + 1
          index = index + 1
        }

        (0 until temp.length) foreach (i => nums(start + i) = temp(i))

        end
      }

      def loop(level: Int = 1): Unit = {
        if (level < nums.length) {
          var next = sort(0, level)
          while (next < nums.length) {
            next = sort(next, level)
          }

          loop(level * 2)
        }
      }

      loop()
      nums
    }

    /*def sortArray(nums: Array[Int]): Array[Int] = {

    def merge(arrayL: Array[Int], arrayR: Array[Int]): Array[Int] = {
      val mergedArray = new Array[Int](arrayL.length + arrayR.length)
      var index = 0
      var l = 0
      var r = 0
      while (l < arrayL.length && r < arrayR.length) {
        if (arrayL(l) <= arrayR(r)) {
          mergedArray(index) = arrayL(l)
          l = l + 1
        } else {
          mergedArray(index) = arrayR(r)
          r = r + 1
        }
        index = index + 1
      }
      while (l < arrayL.length) {
        mergedArray(index) = arrayL(l)
        l = l + 1
        index = index + 1
      }
      while (r < arrayR.length) {
        mergedArray(index) = arrayR(r)
        r = r + 1
        index = index + 1
      }

      mergedArray
    }

    def loop(array: Array[Int]): Array[Int] = {
      if (array.length <= 1) array
      else {
        val mid = array.length / 2
        println(s"mid: $mid")
        val left = loop(array.slice(0, mid))
        val right = loop(array.slice(mid + 1, array.length))
        println("left =" + left.toList)
        println("right=" + right.toList)
        merge(left, right)
      }
    }

    loop(nums)
  }*/
  }
}
