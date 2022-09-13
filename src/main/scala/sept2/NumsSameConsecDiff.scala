package sept2


object NumsSameConsecDiff extends App {

  println(numsSameConsecDiff(3, 7).toList)
  println(numsSameConsecDiff(2, 1).toList)
  println(numsSameConsecDiff(2, 0).toList)

  def numsSameConsecDiff(n: Int, k: Int): Array[Int] = {
    import scala.collection.mutable
    val queue = new mutable.Queue[Int]()
    (1 to 9) foreach (i => queue.enqueue(i))

    var level = 1
    while (level < n) {
      val nodes = queue.dequeueAll(_ => true)
      nodes.foreach { n =>
        val lastDidgit = n % 10

        if (k == 0) queue.enqueue(n * 10 + lastDidgit)
        else {
          if (lastDidgit + k < 10) queue.enqueue(n * 10 + (lastDidgit + k))
          if (lastDidgit - k >= 0) queue.enqueue(n * 10 + (lastDidgit - k))
        }
      }
      level = level + 1
    }
    queue.dequeueAll(_ => true).toArray
  }

}