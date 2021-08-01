package leetcode.ds

import scala.collection.mutable.ArrayBuffer

object Queue extends App {

}


class MyCircularQueue(_k: Int) {

  private val buffer = new ArrayBuffer[Int](_k)
  private var head : Int = -1
  private var tail : Int = -1

  def enQueue(value: Int): Boolean = {

  }

  def deQueue(): Boolean = {

  }

  def Front(): Int = {

  }

  def Rear(): Int = {

  }

  def isEmpty(): Boolean = {
      head == tail == -1
  }

  def isFull(): Boolean = {

  }

}

/**
 * Your MyCircularQueue object will be instantiated and called as such:
 * var obj = new MyCircularQueue(k)
 * var param_1 = obj.enQueue(value)
 * var param_2 = obj.deQueue()
 * var param_3 = obj.Front()
 * var param_4 = obj.Rear()
 * var param_5 = obj.isEmpty()
 * var param_6 = obj.isFull()
 */