package coursera.functional.design.week1

import scala.util.Random


trait Generator_[+T] {
  self => //alias name

  def generate: T

  def map[S](f: T => S): Generator_[S] = new Generator_[S] {
    //Here, f(this.generate) | f(generate) will do a recursive call
    //Using alias-name(self) self.generate refers to the outer trait's generate method.
    override def generate: S = f(self.generate) //use of alias
  }

  def flatMap[S](f: T => Generator_[S]) = new Generator_[S] {
    override def generate: S = f(self.generate).generate
  }
}


object Generator_Instances {

  val integer = new Generator_[Int] {
    override def generate: Int = new Random().nextInt();
  }

  val bool = new Generator_[Boolean] {
    override def generate: Boolean = integer.generate > 0
  }

  val t2 = new Generator_[(Int, Int)] {
    override def generate: (Int, Int) = integer.generate -> integer.generate
  }

  //using map/flatMap:
  val boolNew = integer.map(i => i > 0)

  val t2New: Generator_[(Int, Int)] = integer.flatMap(i => integer map { j => i -> j })


  def single[S](x: S): Generator_[S] = new Generator_[S] {
    override def generate: S = x
  }

  def chooseBetween(l: Int, h: Int): Generator_[Int] = new Generator_[Int] {
    override def generate: Int = (l + (h - l) / 2)
  }

  def oneOf[S](x: S*): Generator_[S] = new Generator_[S] {
    override def generate: S = x(chooseBetween(0, x.length).generate)
  }
}