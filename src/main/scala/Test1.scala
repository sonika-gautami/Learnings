/*


object Test1 extends App {

  val l = Seq(1, 2, 3)
  val l2 = l.map(_ * 2)
  println(l2)

  val s = Seq(1,2,3)
  s.map(i => (i, 10)).groupBy(_._1).mapValues(l -> l.sum)


  val mutiply: (Int => Int) = i => i * 2

  def mymap(f: Int => Int)(list: Seq[Int]): Seq[Int] = {
    val op = new scala.collection.mutable.MutableList[Int]()

    list.foreach(i => {
      op.+=:(f(i))
    })


    op.toSeq
  }


  val l3 = mymap(mutiply)(l)
  println(l3)

}
*/
