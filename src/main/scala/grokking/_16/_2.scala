package grokking._16

object _2 extends App {
  val w = Array(2, 3, 1, 4)
  val p = Array(4, 5, 3, 7)
  val knapsackCapacity = 5

  println(burstForce(w, p, knapsackCapacity).toList)
  println(burstForce2(w, p, knapsackCapacity).toList)
  println(burstForceRec(w, p, knapsackCapacity))

  //not-working
  //Only Count, Max Profit with Min items-picked
  def burstForceRec(w: Array[Int], p: Array[Int], max: Int): Int = {
    def loop(index: Int, remainingCapacity: Int): Int = {
      if (remainingCapacity <= 0 || index >= w.length) 0
      else {
        val profitNotPickingCurr = loop(index + 1, remainingCapacity)
        if (p(index) <= remainingCapacity) {
          val profitPickingCurr = p(index) + loop(index + 1, remainingCapacity - p(index))
          Math.max(profitPickingCurr, profitNotPickingCurr)
        } else profitNotPickingCurr
      }
    }

    loop(0, max)
  }

  //working
  def burstForce(w: Array[Int], p: Array[Int], max: Int): Array[Int] = {
    //try all combinations
    // for 4 items picked by 1 time only (0th time or 1 time = 2 possibilities),
    //  we will have 2^4 = 16 combinations
    var profitMax = 0
    var selectedOnes: List[Int] = Nil

    def reachedMaxOrTriedAll(accSelected: List[Int], accProfit: Int): Unit = {
      if (accProfit > profitMax) {
        profitMax = accProfit
        selectedOnes = accSelected
      }
    }

    def loop(index: Int, accSelected: List[Int], accProfit: Int): Unit = {
      if (index >= w.length) {
        reachedMaxOrTriedAll(accSelected, accProfit)
      } else {
        if (accSelected.sum + w(index) <= max)
          loop(index + 1, accSelected :+ (w(index)), accProfit + p(index)) //picking current item
        else
          reachedMaxOrTriedAll(accSelected, accProfit)

        loop(index + 1, accSelected, accProfit) //not-picking current item
      }
    }

    loop(0, List(), 0)
    selectedOnes.toArray
  }

  //working
  def burstForce2(w: Array[Int], p: Array[Int], max: Int): Array[Int] = {
    //try all combinations
    // for 4 items picked by 1 time only (0th time or 1 time = 2 possibilities),
    //  we will have 2^4 = 16 combinations
    var profitMax = 0
    var selectedOnes: List[Int] = Nil

    def reachedMaxOrTriedAll(accSelected: List[Int], accProfit: Int): Unit = {
      if (accProfit > profitMax) {
        profitMax = accProfit
        selectedOnes = accSelected
      }
    }

    def loop(index: Int, accSelected: List[Int], accProfit: Int): Unit = {
      if (index >= w.length) {
        reachedMaxOrTriedAll(accSelected, accProfit)
      } else {
        // as we're closing this loop early, we should check against accumulators
        if (accSelected.sum + w(index) > max) reachedMaxOrTriedAll(accSelected, accProfit)

        // A while loop to allow to pick many times as-well; instead just 0 | 1
        var i = 0
        var currSelected: List[Int] = Nil
        while (accSelected.sum + (i * w(index)) <= max && i <= 1) {
          loop(index + 1, accSelected ++ currSelected, accProfit + (i * p(index)))

          currSelected = currSelected :+ (w(index))
          i = i + 1
        }
      }
    }

    loop(0, List(), 0)
    selectedOnes.toArray
  }

}
