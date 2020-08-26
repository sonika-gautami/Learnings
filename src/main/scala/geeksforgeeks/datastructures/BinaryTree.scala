package geeksforgeeks.datastructures


object BinaryTree extends App {

  //Hierarchical data structures:
  /*

    ////////////
    Binary Tree:
    - each node has at most two children, left child and the right child.
      It is implemented mainly using Link
    - represented by ROOT node pointer

    Traversal of Binary-Tree:
    1] BFS [Breadth First Traversal]
        - Level Order

    2] DFS [Depth First Traversal]
        - In-Order    [ Left-Root-Right ]
        - Pre-Order   [ Root-Left-Right ]
        - Post-Order  [ Left-Right-Root ]

    Formulas:
    Max no of nodes at level-n  = 2^(n-1)

    Max no of nodes = 2^(h+1) - 1
    h = Height of Tree [ Max no of edges on a path from root to leaf.]

    Min possible height =  ceil( Log2(n+1) ) - 1


    Usage:
    - things that form a hierarchy
      i.e File Structures (dir-files path)
          Html tags (parent tag -> child tags)


    ////////////
    Binary Search Tree:
    Binary tree with:
      - left subtree values < right
      - right subtree values > left
      - left & right of a node is also a binary search tree

   - If height balanced then, h = o(log(n))

   - BST provide moderate access/search (quicker than Linked List and slower than arrays)
   - BST provide moderate insertion/deletion (quicker than Arrays and slower than Linked Lists)

   Usage:
   - data is constantly inserting/leaving & it needs to be in sorted order
     i.e. E- commerce websites
            where a new product is added or product goes out of stock and
            all products are lised in sorted order.


  ////////////
  Binary Heap:
  - A Complete Tree
    - All levels are completely filled except (possibly) last one
    - Last Level has all keys possibly as left as possible
  - Suitable to store as Array

  2 Types:
  1] Min Heap:
      - Root Key must be less than all its childs

  2] max Heap:
      - Root Key must be greater than all its childs

  Usage:
  - implementing efficient priority queues
    i.e scheduling processes in OS
  - efficiently find the k largest/smallest elements in array
  - merging k sorted array
  - median of a stream

  -> can NOT be used for searching of a particular element


   */
}
