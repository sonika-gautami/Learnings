package leetcode.ds

object Recursion2Part2 extends App {

  {
    println(generateParenthesis(1))
    println(generateParenthesis(2))
    println(generateParenthesis(3))
    println(generateParenthesis(4))

    def generateParenthesis(n: Int): List[String] = {
        def valid(str: String) = {
          var count = 0
          var i = 0
          while (count >= 0 && i < str.length) {
            if (str(i) == '(') count = count + 1 else count = count - 1
            i = i + 1
          }
          count == 0
        }

        def loop(i: Int, acc: String, prevCount: Int): List[String] = {
          if (i == 2 * n - 1) List(acc + "(", acc + ")").filter(valid)
          else
            loop(i + 1, acc + "(", prevCount + 1) ++
              (if (prevCount - 1 >= 0) loop(i + 1, acc + ")", prevCount - 1) else Nil)
        }

        loop(0, "", 0)
    }
  }

  {
    var n = 4
    var sol: List[String] = Nil
    sol = allCombinations(n)
    println(sol)
    assert(sol.length == factorial(n))

    n = 3
    sol = allCombinations(n)
    println(sol)
    assert(sol.length == factorial(n))

    def allCombinations(n: Int): List[String] = {
      val all = 1 to n

      def loop(i: Int, acc: List[String]): List[String] = {
        if (i == n) acc
        else loop(
          i + 1,
          acc.flatMap { pre =>
            all.filter(no => !pre.contains(no.toString)).map(no => pre + no)
          })
      }

      loop(1, (1 to n).map(_.toString).toList)
    }
  }

  def factorial(n: Int): Int = if (n == 1) 1 else n * factorial(n - 1)
}
