package grokking._16

object CoinChange extends App {

  println(coinChange(coins = Array(1, 2, 5), amount = 11))
  println(coinChange(coins = Array(2), amount = 3))
  println(coinChange(coins = Array(1), amount = 0))

  //fewest no of coins - min
  //if not possible return -1
  def coinChange(coins: Array[Int], amount: Int): Int = {
    //Not working this  optimization:
    //brust-force - one optimization can be sort the coins array n - logn
    //do brust-force until we hit first solution (as we've sorted already we can take
    // first one as min required currency)

    val coinsSorted = coins.sortWith((i1, i2) => i1 > i2)
    println(coinsSorted.toList)
    var minCoinsCount = Int.MaxValue

    def checkSolution(accSelected: Int, accAmount: Int): Boolean = {
      if (accAmount == amount) {
        println(s"$accSelected - $accAmount")
        if (minCoinsCount > accSelected) {
          minCoinsCount = accSelected
          return true
        }
      }
      false
    }

    def loop(index: Int, accSelected: Int, accAmount: Int): Unit = {
      if (index == coinsSorted.length) checkSolution(accSelected, accAmount)
      else {
        if (accAmount == amount) checkSolution(accSelected, accAmount)

        var i = 0
        while ((accAmount + (i * coinsSorted(index))) <= amount) {
          loop(index + 1, accSelected + i, accAmount + (i * coinsSorted(index)))
          i = i + 1
        }
      }
    }

    if (amount == 0) 0
    else {
      loop(0, 0, 0)
      if (minCoinsCount == Int.MaxValue) -1 else minCoinsCount
    }
  }

}
