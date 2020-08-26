package geeksforgeeks.datastructures

import org.apache.spark.util.SizeEstimator

//import com.madhukaraphatak.sizeof.SizeEstimator

object LinearDS extends App {

  /*

  Heap memory -> at run-time
  Data/Stack memory -> as allocated (like Long lArr = new Long[5])

  Array:
  - contiguous memory allocated
  - memory section allocated can be -> @data @stack or @heap
  - Due to contiguous memory for successive elements in the array,
     no extra information is needed to be stored in individual elements
    i.e. no overhead of metadata in arrays
  - The idea is to store multiple items of the same type together.
    This makes it easier to calculate the position of each element
     by simply adding an offset to a base value,
    i.e., the memory location of the first element of the array
          (generally denoted by the name of the array).

  - Pros:
    - Allow Random Access (Faster By Position)
    - Array have better Cache locality make a pretty big difference in performance.

  LinkedList:
  - non-contiguous linked memory
  - memory section allocated can be -> @data @stack or @heap
  - linked list nodes are non-contiguous in memory.
     so we need some mechanism to traverse or access linked list nodes.
    storing the location of the next node is overhead in linked list

  Stack:
  - can be implemented with Array Or LinkedList
  - Usages:
    - we can always remove recursion with stacks
    - UNDO functionality
    - BACK buttom functionality
    - Balanced parenthesis
    - Reverse a word


  Queue:
  - can be implemented with Array Or LinkedList
  - Usages:
    - resources are shared based on first come first serve basis:
      i.e CPU scheduling, Disk Scheduling
    - data is transferred asynchronously between two processes
      (data not received as same rate as sent)
      i.e IO Buffers, pipes, file IO


  Custom solutions can be used:
  Arrays of LinkedList:
  Ref: https://www.geeksforgeeks.org/programmers-approach-looking-array-vs-linked-list/
   */

  def printSize[T <: AnyRef](obj: T) =
    println(s"obj of: ${obj.getClass} size_of: ${SizeEstimator.estimate(obj)/4}")


  printSize(Array[Int](1))
  printSize(List[Int](1))

  printSize(Array[Int](1, 2))
  printSize(List[Int](1, 2))

  printSize(Array[Int](1, 2, 3))
  printSize(List[Int](1, 2, 3))

  printSize(Array[Int](1, 2, 3, 4))
  printSize(List[Int](1, 2, 3, 4))
}
