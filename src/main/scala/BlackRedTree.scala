/**
  * @author Maria Swianiewicz
  * @author Karolina Deszcz
  */

import scala.annotation.tailrec


/** Abstract class which represents node's colors.
  * A node can be either black or red. */

abstract class Color

case object Red extends Color

case object Black extends Color

/** Auxiliary color, used in deletion algorithm. Counts twice when root-leaf path is calculated */
case object DoubleBlack extends Color

/** Auxiliary color, used in deletion algorithm. */
case object NegativeBlack extends Color

/** Black Red Tree class.
  * It's a binary tree, which fullfills additional constraints:
  *     - root is black
  *     - leaves are black
  *     - each red node has two black children
  *     - number of black nodes in each path from root to any leaf node has to be the same
  *
  */
sealed abstract class RBTree[+A] {
  def color: Color
}

/** Leaf object
  * empty trees are represented by Leaf objects.
  *
  */

case object Leaf extends RBTree[Nothing] {
  def color: Color = Black
}

/** LeafDoubleBlack object
  * Auxiliary object used in deletion algorithm
  */

case object LeafDoubleBlack extends RBTree[Nothing] {
  def color: Color = DoubleBlack
}

/** Node class. Nodes are immutable
  *
  * @param color node color
  * @param value value stored in a node
  * @param left  node's left child
  * @param right node's right child
  * @tparam A value type
  */
case class Node[+A](color: Color = Black, value: A, left: RBTree[A] = Leaf, right: RBTree[A] = Leaf) extends RBTree[A] {
  def this(value: A) = this(Black, value, Leaf, Leaf)
}


/** main Black red tree object. Provides methods performed on trees.
  */

object RBTree {

  /**
    * constructor which creates Node with leaves as its children
    *
    * @param value value stored in a node
    * @tparam A value type
    * @return new Node
    */
  def apply[A](value: A): Node[A] = new Node(value)

  /**
    * returns height of the tree. Height of a leaf is 0.
    *
    * @param tree tree object which hight is beeing calculated
    * @tparam A data type
    * @return height od a tree
    */
  def height[A](tree: RBTree[A]): Int = {
    tree match {
      case Leaf => 0
      case Node(_, _, left, right) => 1 + math.max(height(left), height(right))
    }
  }

  /**
    * inserts a new element to existing tree. If value already exists in a tree, the tree is returned with no changes.
    * After insertion all color-constrains are preserved.
    *
    * @param x    value to be added to the tree
    * @param tree tree to which the value will be added
    * @tparam A value type
    * @return new black red tree with element added
    */
  def insert[A: Ordering](x: A)(tree: RBTree[A]): RBTree[A] = {
    blacken(ins(x)(tree))
  }

  private def blacken[A](tree: RBTree[A]): RBTree[A] = {
    tree match {
      case Leaf => tree
      case LeafDoubleBlack => Leaf
      case Node(_, value, left, right) => Node(Black, value, left, right)
    }
  }
  /**
    * Auxiliary method, adds element to the tree. After insertion color constraints are violated.
    *
    * @param x    value to be added to the tree
    * @param tree tree to which the value will be added
    * @tparam A value type
    * @return new tree with element added (with color constraints violated)
    */

  private def ins[A: Ordering](x: A)(tree: RBTree[A]): RBTree[A] = {
    tree match {
      case Leaf => Node(Red, x, Leaf, Leaf)
      case Node(color, value, left, right) =>
        if (Ordering[A].compare(x, value) < 0) balance(color, value, ins(x)(left), right)
        else if (Ordering[A].compare(x, value) > 0) balance(color, value, left, ins(x)(right))
        else tree
    }
  }

  /** checks if value exists in a tree. If exists returns true, otherwise returns false
    *
    * @param tree tree in which value is searched
    * @param x    searched value
    * @tparam A data type
    * @return true if value exists otherwise false
    */

  @tailrec
  def contains[A: Ordering](tree: RBTree[A])(x: A): Boolean = {
    tree match {
      case Leaf => false
      case Node(_, value, left, right) =>
        if (Ordering[A].compare(x, value) < 0) contains(left)(x)
        else if (Ordering[A].compare(x, value) > 0) contains(right)(x)
        else true
    }
  }


  /** balances a tree. Depending on the tree being balanced performs left or right rotation.
    *
    * @param color node's color
    * @param value node's value
    * @param left  node's left child
    * @param right node's right child
    * @tparam A data type
    * @return balanced tree
    */

  private def balance[A](color: Color, value: A, left: RBTree[A], right: RBTree[A]): RBTree[A] =
    (color, value, left, right) match {
      case (Black, z, Node(Red, y, Node(Red, x, a, b), c), d) =>
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (Black, z, Node(Red, x, a, Node(Red, y, b, c)), d) =>
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (Black, x, a, Node(Red, z, Node(Red, y, b, c), d)) =>
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (Black, x, a, Node(Red, y, b, Node(Red, z, c, d))) =>
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))

      case (DoubleBlack, x, a, Node(Red, z, Node(Red, y, b, c), d)) =>
        Node(Black, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (DoubleBlack, z, Node(Red, x, a, Node(Red, y, b, c)), d) =>
        Node(Black, y, Node(Black, x, a, b), Node(Black, z, c, d))

      case (c, x, a, b) => Node(c, x, a, b)
    }

  /**
    * makes node with two black children red
    * @param tree tree
    * @tparam A data type
    * @return modified node
    */
  private def redden[A](tree: RBTree[A]): RBTree[A] = {
    tree match {
      case Node(Black, x, Node(Black, y, a, b), Node(Black, z, c, d)) =>
        Node(Red, x, Node(Black, y, a, b), Node(Black, z, c, d))
      case LeafDoubleBlack => Leaf
      case _ => tree
    }
  }

  /**
    * performs rotation
    * @param color node's color
    * @param value node's value
    * @param left node's left child
    * @param right node's right child
    * @tparam A data type
    * @return tree after rotation
    */

  private def rotate[A](color: Color, value: A, left: RBTree[A], right: RBTree[A]): RBTree[A] = {
    (color, value, left, right) match {
      case (Red, y, Node(DoubleBlack, x, a, b), Node(Black, z, c, d)) =>
        balance(Black, z, Node(Red, y, Node(Black, x, a, b), c), d)
      case (Red, y, LeafDoubleBlack, Node(Black, z, c, d)) =>
        balance(Black, z, Node(Red, y, Leaf, c), d)
      case (Red, y, Node(Black, x, a, b), Node(DoubleBlack, z, c, d)) =>
        balance(Black, x, a, Node(Red, y, b, Node(Black, z, c, d)))
      case (Red, y, Node(Black, x, a, b), LeafDoubleBlack) =>
        balance(Black, x, a, Node(Red, y, b, Leaf))
      case (Black, y, Node(DoubleBlack, x, a, b), Node(Black, z, c, d)) =>
        balance(DoubleBlack, z, Node(Red, y, Node(Black, x, a, b), c), d)
      case (Black, y, LeafDoubleBlack, Node(Black, z, c, d)) =>
        balance(DoubleBlack, z, Node(Red, y, Leaf, c), d)
      case (Black, y, Node(Black, x, a, b), Node(DoubleBlack, z, c, d)) =>
        balance(DoubleBlack, x, a, Node(Red, y, b, Node(Black, z, c, d)))
      case (Black, y, Node(Black, x, a, b), LeafDoubleBlack) =>
        balance(DoubleBlack, x, a, Node(Red, y, b, Leaf))
      case (Black, x, Node(DoubleBlack, w, a, b), Node(Red, z, Node(Black, y, c, d), e)) =>
        Node(Black, z, balance(Black, y, Node(Red, x, Node(Black, w, a, b), c), d), e)
      case (Black, x, LeafDoubleBlack, Node(Red, z, Node(Black, y, c, d), e)) =>
        Node(Black, z, balance(Black, y, Node(Red, x, c, Leaf), d), e)
      case (Black, y, Node(Red, w, a, Node(Black, x, b, c)), Node(DoubleBlack, z, d, e)) =>
        Node(Black, w, a, balance(Black, x, b, Node(Red, y, c, Node(Black, z, d, e))))
      case (Black, y, Node(Red, w, a, Node(Black, x, b, c)), LeafDoubleBlack) =>
        Node(Black, w, a, balance(Black, x, b, Node(Red, y, c, Leaf)))
      case (c, x, a, b) => Node(c, x, a, b)
    }
  }

  /**
    * deletes min value in subtree
    * @param tree tree
    * @tparam A data type
    * @return tuple - (minimum value, tree without that value)
    */
  def minDel[A: Ordering](tree: RBTree[A]): (A, RBTree[A]) = {
    tree match {
      case Node(Red, x, Leaf, Leaf) => (x, Leaf)
      case Node(Black, x, Leaf, Leaf) => (x, LeafDoubleBlack)
      case Node(Black, x, Leaf, Node(Red, y, Leaf, Leaf)) => (x, Node(Black, y, Leaf, Leaf))
      case Node(color, x, a, b) => {
        val (y, c)  = minDel(a)
        (y, rotate(color, x, c, b))
      }
    }
  }

  /**
    * deletes a value from the tree. After deletion tree preserves all color constraints
    * @param value value that will be removed
    * @param tree tree from which value will be removed
    * @tparam A data type
    * @return tree without value
    */
  def delete[A: Ordering](value: A)(tree: RBTree[A]): RBTree[A] = {
    if (!contains(tree)(value)) fail("value doesn't exist")
    blacken(del(value)(redden(tree)))
  }

  /**
    * auxiliary method, deletes value from the tree. After deletion color constraints may be violated
    *@param value value that will be removed
    * @param tree tree from which value will be removed
    * @tparam A data type
    * @return tree without value
    */
  private def del[A: Ordering](value: A)(tree: RBTree[A]): RBTree[A] = {
    tree match {
      case Leaf => Leaf
      case Node(Red, y, Leaf, Leaf) =>
        if (Ordering[A].compare(value, y) == 0) Leaf
        else tree
      case Node(Black, y, Leaf, Leaf) =>
        if (Ordering[A].compare(value, y) == 0) LeafDoubleBlack
        else tree
      case Node(Black, z, Node(Red, y, Leaf, Leaf), Leaf) =>
        if (Ordering[A].compare(value, z) < 0) Node(Black, z, del(value)(Node(Red, y, Leaf, Leaf)), Leaf)
        else if (Ordering[A].compare(value, z) > 0) tree
        else Node(Black, y, Leaf, Leaf)
      case Node(color, y, l, r) =>
        if (Ordering[A].compare(value, y) < 0) rotate(color, y, del(value)(l), r)
        else if (Ordering[A].compare(value, y) > 0) rotate(color, y, l, del(value)(r))
        else {
          val (x, b)  = minDel(r)
          rotate(color, x, l, b)
        }
    }
  }

  /** returns union of two trees.
    *
    * @param tree  tree on which union will be performed
    * @param other tree on which union will be performed
    * @tparam A data type
    * @return tree which contains all values from both trees
    */
  @tailrec
  def union[A: Ordering](tree: RBTree[A])(other: RBTree[A]): RBTree[A] = {
    tree match {
      case Leaf => other
      case Node(_, value, _, _) =>
       union(delete(value)(tree))(insert(value)(other))
    }
  }

  /** returns intersection of two trees.
    * Returned tree is balanced and preserves all black-red constraints
    * @param tree  tree on which intersection will be performed
    * @param other tree on which intersection will be performed
    * @tparam A data type
    * @return tree which contains only those values which are present in both trees
    */
  def intersection[A: Ordering](tree: RBTree[A])(other: RBTree[A]): RBTree[A] = {
    intersect(tree)(other)()
  }

  /**
    * Auxiliary method, calculates intersection of two trees.
    * Returned tree preserves all black-red constraints
    *
    * @param tree   tree on which intersection will be performed, common elements are deleted from this tree
    * @param other  tree on which intersection will be performed
    * @param result auxiliary tree, common elements in both trees are added to the result tree
    * @tparam A data type
    * @return tree with elements that exist in both trees
    */
  @tailrec
  private def intersect[A: Ordering](tree: RBTree[A])(other: RBTree[A])(result: RBTree[A] = Leaf): RBTree[A] = {
    tree match {
      case Leaf => result
      case Node(_, value, _, _) =>
        if (contains(other)(value)) {
          intersect(delete(value)(tree))(other)(insert(value)(result))
        }
        else {
          intersect(delete(value)(tree))(other)(result)
        }
    }
  }

  /** throws an exception, when invalid operation is performed
    *
    * @param msg exception message
    * @return new exception
    */
  private def fail(msg: String) = throw new Exception(msg)


  /**
    * calculates the maximum length of black-nodes path from root to leaves
    * @param tree tree which path length will be calculated
    * @tparam A data type
    * @return length of the longest black-node path
    */
  private def maxBlackPath[A](tree: RBTree[A]):Int = {
    tree match {
      case Leaf => 1
      case Node (color,_,left,right) =>
        if (color == Black) 1 + math.max(maxBlackPath(left), maxBlackPath(right))
        else math.max(maxBlackPath(left), maxBlackPath(right))
    }
  }

  /**
    * calculates the minimum length of black-nodes path from root to leaves
    * @param tree tree which path length will be calculated
    * @tparam A data type
    * @return length of the shortest black-node path
    */
  private def minBlackPath[A](tree: RBTree[A]):Int = {
    tree match {
      case Leaf => 1
      case Node (color,_,left,right) =>
        if (color == Black) 1 + math.min(minBlackPath(left), minBlackPath(right))
        else math.min(minBlackPath(left), minBlackPath(right))
    }
  }

  /**
    * checks if all paths from root to leaves have the same number of black nodes
    * @param tree tree
    * @tparam A data type
    * @return true if all paths from root to leaves have the same number of black nodes, false otherwise
    */
  private def isBlackBalanced[A](tree:RBTree[A]):Boolean = {
    if (maxBlackPath(tree) == minBlackPath(tree)) true
    else false
  }

  /**
    * checks if exists a red node with doesn't have two black children, which would violate red-black tree constraint
    * @param tree tree
    * @tparam A data type
    * @return true if such node exists, false otherwise
    */
  private def hasAnyRedNodeRedChildren[A](tree:RBTree[A]):Boolean = {
    tree match {
      case Leaf => false
      case Node(Black,_,left,right) => hasAnyRedNodeRedChildren(left) && hasAnyRedNodeRedChildren(right)
      case Node(Red,_,left,right) =>
        if (left.color == Black) hasAnyRedNodeRedChildren(left) && hasAnyRedNodeRedChildren(right)
        else if (right.color == Black) hasAnyRedNodeRedChildren(left) && hasAnyRedNodeRedChildren(right)
        else true
    }
  }

  /**
    * checks if tree fullfills red-black tree constraints
    * @param tree tree
    * @tparam A data type
    * @return true if tree is Red-Black, false otherwise
    */

  def isTreeBlackRed[A](tree:RBTree[A]):Boolean = {
    tree match {
      case Node(color,_,_,_) => color == Black && !hasAnyRedNodeRedChildren(tree) && isBlackBalanced(tree)
      case Leaf => true
    }
  }

}

