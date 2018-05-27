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
  *   - root is black
  *     - leaves are black
  *     - each red node has two black children
  *     - number of black nodes in each path from root to any leaf node has to be the same
  *
  */
sealed abstract class BRTree[+A] {
  def color: Color
}

/** Leaf object
  * empty trees are represented by Leaf objects.
  *
  */

case object Leaf extends BRTree[Nothing] {
  def color: Color = Black
}

/** LeafDoubleBlack object
  * Auxiliary object used in deletion algorithm
  */

case object LeafDoubleBlack extends BRTree[Nothing] {
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
case class Node[+A](color: Color = Black, value: A, left: BRTree[A] = Leaf, right: BRTree[A] = Leaf) extends BRTree[A] {
  def this(value: A) = this(Black, value, Leaf, Leaf)
}


/** main Black red tree object. Provides methods performed on trees.
  */

object BRTree {

  /**
    * constructor which creates Node with leaves as its children
    *
    * @param value value stored in a node
    * @tparam A value type
    * @return new Node
    */
  def apply[A](value: A): Node[A] = new Node(value)

  /**
    * method which returns height of the tree. Height of a leaf is 0.
    *
    * @param tree tree object which hight is beeing calculated
    * @tparam A data type
    * @return height od a tree
    */
  def height[A](tree: BRTree[A]): Int = {
    tree match {
      case Leaf => 0
      case Node(_, _, left, right) => 1 + math.max(height(left), height(right))
    }
  }

  /**
    * method which inserts a new element to existing tree. If value already exists in a tree, the tree is returned with no changes.
    * After insertion tree is balanced and all color-constrains are preserved.
    *
    * @param x    value to be added to the tree
    * @param tree tree to which the value will be added
    * @tparam A value type
    * @return new black red tree with element added
    */
  def insert[A: Ordering](x: A)(tree: BRTree[A]): BRTree[A] = {
    blacken(ins(x)(tree))
  }

  /**
    * Auxiliary method which adds element to the tree. After insertion tree is balanced, however color constraints are violated.
    *
    * @param x    value to be added to the tree
    * @param tree tree to which the value will be added
    * @tparam A value type
    * @return new tree with element added (with color constraints violated)
    */

  private def ins[A: Ordering](x: A)(tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Leaf => Node(Red, x, Leaf, Leaf)
      case Node(_, value, left, right) =>
        if (Ordering[A].compare(x, value) > 0) balance(Black, x, ins(value)(left), right)
        else if (Ordering[A].compare(x, value) < 0) balance(Black, x, left, ins(value)(right))
        else tree
    }
  }

  /** method balancing a tree. Depending on the tree being balanced performs left or right rotation.
    *
    * @param color node's color
    * @param value node's value
    * @param left  node's left child
    * @param right node's right child
    * @tparam A data type
    * @return balanced tree
    */
  private def balance[A](color: Color, value: A, left: BRTree[A], right: BRTree[A]): BRTree[A] =
    (color, value, left, right) match {
      case (Black, z, Node(Red, y, Node(Red, x, a, b), c), d) =>
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (Black, z, Node(Red, x, a, Node(Red, y, b, c)), d) =>
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (Black, x, a, Node(Red, z, Node(Red, y, b, c), d)) =>
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (Black, x, a, Node(Red, y, b, Node(Red, z, c, d))) =>
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))

      case (DoubleBlack, z, Node(Red, y, Node(Red, x, a, b), c), d) =>
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (DoubleBlack, z, Node(Red, x, a, Node(Red, y, b, c)), d) =>
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (DoubleBlack, x, a, Node(Red, z, Node(Red, y, b, c), d)) =>
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (DoubleBlack, x, a, Node(Red, y, b, Node(Red, z, c, d))) =>
        Node(Red, y, Node(Black, x, a, b), Node(Black, z, c, d))
      case (DoubleBlack, x, a, Node(NegativeBlack, z, Node(Black, y, b, c), d)) if d.color == Black =>
        Node(Black, y, Node(Black, x, a, b), balance(Black, z, c, redden(d)))
      case (DoubleBlack, z, Node(NegativeBlack, x, a, Node(Black, y, b, c)), d) if a.color == Black =>
        Node(Black, y, balance(Black, x, redden(a), b), Node(Black, z, c, d))

      case (c, x, a, b) => Node(c, x, a, b)
    }

  /** Auxiliary method, changes node's color to black
    *
    * @param tree tree which color will be changed
    * @tparam A data type
    * @return tree node with black color
    */
  private def blacken[A](tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Leaf => tree
      case LeafDoubleBlack => Leaf
      case Node(_, value, left, right) => Node(Black, value, left, right)
    }
  }

  /** Auxiliary method, changes node's color to red.
    * On attempt of changing Leaf color to red an exception will be thrown (that would violate red-black constraints)
    *
    * @param tree tree which color will be changed
    * @tparam A data type
    * @return tree node with black color
    */
  private def redden[A](tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Leaf => fail("it's not possible to make Leaf red")
      case LeafDoubleBlack => fail("it's not possible to make DoubleBlackLeaf red")
      case Node(_, value, left, right) => Node(Red, value, left, right)
    }
  }

  /** Auxiliary method which performs "color math", makes color blacker, according to below rules:
    *   - NegativeBlack => Red
    *   - Red => Black
    *   - Black => DoubleBlack
    * On attempt of blacken DoubleBlack an exception will be thrown
    *
    * @param color color that will be changed
    * @return Color
    */

  private def blacker(color: Color): Color = {
    color match {
      case NegativeBlack => Red
      case Red => Black
      case Black => DoubleBlack
      case DoubleBlack => fail("it's not possible to make DoubleBlack blacker")
    }
  }

  /** Auxiliary method which performs "color math", makes color redder, according to below rules:
    *   - DoubleBlack => Black
    *   - Black => Red
    *   - Red => NegativeBlack
    * On attempt of redden DoubleBlack an exception will be thrown
    *
    * @param color color that will be changed
    * @return Color
    */
  private def redder(color: Color): Color = {
    color match {
      case NegativeBlack => fail("it's not possible to make NegativeBlack redder")
      case Red => NegativeBlack
      case Black => Red
      case DoubleBlack => Black
    }
  }

  /** Auxiliary method, makes tree 'blacker' - performs blacker method on node's color
    *
    * @param tree tree which color will be changed
    * @tparam A data type
    * @return tree
    */

  private def blacker[A](tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Leaf => LeafDoubleBlack
      case LeafDoubleBlack => fail("it's not possible to make DoubleBlackLeaf blacker")
      case Node(color, value, left, right) => Node(blacker(color), value, left, right)
    }
  }

  /** Auxiliary method, makes tree 'redder' - performs redder method on node's color
    *
    * @param tree tree which color will be changed
    * @tparam A data type
    * @return tree
    */

  private def redder[A](tree: BRTree[A]): BRTree[A] = {
    tree match {
      case LeafDoubleBlack => Leaf
      case Leaf => fail("it's not possible to make Leaf redder")
      case Node(color, value, left, right) => Node(redder(color), value, left, right)
    }
  }

  /** Method which checks if value exists in a tree. If exists returns true, otherwise returns false
    *
    * @param tree tree in which value is searched
    * @param x    searched value
    * @tparam A data type
    * @return true if value exists otherwise false
    */

  @tailrec
  def contains[A: Ordering](tree: BRTree[A])(x: A): Boolean = {
    tree match {
      case Leaf => false
      case Node(_, value, left, right) =>
        if (Ordering[A].compare(x, value) < 0) contains(left)(x)
        else if (Ordering[A].compare(x, value) > 0) contains(right)(x)
        else true
    }
  }

  /** Method which deletes value from a given tree. After deletion tree is balanced and preseres
    * black-red constraints
    *
    * @param value value to be deleted
    * @param tree  tree from which element will be removed
    * @tparam A data type
    * @return tree with removed value
    */
  def delete[A: Ordering](value: A)(tree: BRTree[A]): BRTree[A] = {
    blacken(del(value)(tree))
  }

  /** Auxiliary method. Performs deletion, resulting tree is balanced but doesn't
    * preserve black-red constraints
    *
    * @param value value to be deleted
    * @param tree  tree from which element will be removed
    * @tparam A data type
    * @return tree with removed value
    */
  private def del[A: Ordering](value: A)(tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Leaf => Leaf
      case Node(c, y, l, r) =>
        if (Ordering[A].compare(value, y) < 0) bubble(c, y, del(value)(l), r)
        else if (Ordering[A].compare(value, y) > 0) bubble(c, y, l, del(value)(r))
        else remove(tree)
    }
  }

  /** Auxiliary method. Performs deletion, resulting tree is unbalanced and doesn't
    * preserve black-red constraints.
    *
    * @param tree tree from which element will be removed
    * @tparam A data type
    * @return tree with removed value
    */
  private def remove[A: Ordering](tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Leaf => Leaf
      case Node(Red, _, Leaf, Leaf) => Leaf
      case Node(Black, _, Leaf, Leaf) => LeafDoubleBlack
      case Node(Black, _, Leaf, Node(Red, x, left, right)) => Node(Black, x, left, right)
      case Node(Black, _, Node(Red, x, left, right), Leaf) => Node(Black, x, left, right)
      case Node(color, _, left, right) => bubble(color, max(left), removeMax(left), right)
    }
  }

  /** Auxiliary method. Deletes max value in a given tree, resulting tree is unbalanced and doesn't
    * preserve black-red constraints.
    *
    * @param tree tree from which max value will be removed
    * @tparam A data type
    * @return tree with removed max value
    */

  private def removeMax[A: Ordering](tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Node(_, _, _, Leaf) => remove(tree)
      case Node(color, x, left, right) => bubble(color, x, left, removeMax(right))
    }
  }

  /** Auxiliary method. If one of tree's children is DoubleBlack makes tree blacker and its children redder.
    * Then performs balancing. This operation helps to eliminate DoubleBlack nodes, or moves them upward.
    *
    * @param color node's color
    * @param value node's value
    * @param left  node's left child
    * @param right node's right child
    * @tparam A data type
    * @return balanced tree, with no DoubleBlack leaves
    */

  private def bubble[A](color: Color, value: A, left: BRTree[A], right: BRTree[A]): BRTree[A] = {
    if (isDoubleBlack(left) || isDoubleBlack(right)) balance(blacker(color), value, redder(left), redder(right))
    else balance(color, value, left, right)
  }


  /** Auxiliary method. Checks if tree color is DoubleBlack
    *
    * @tparam A data type
    * @return true if tree is DoubleBlack, false otherwise
    */
  private def isDoubleBlack[A](tree: BRTree[A]): Boolean = {
    tree match {
      case LeafDoubleBlack => true
      case Node(DoubleBlack, _, _, _) => true
      case _ => false
    }
  }

  /** Method which returns max value in a tree
    * On attempt of finding max in Leaf exception is thrown
    *
    * @param tree tree in which max is searched
    * @tparam A data type
    * @return max value in a tree
    */

  @tailrec
  private def max[A](tree: BRTree[A]): A = {
    tree match {
      case Leaf => fail("leaf has no max value")
      case Node(_, x, _, Leaf) => x
      case Node(_, _, _, r) => max(r)
    }
  }

  /** Method which returns union of two trees.
    *
    * @param tree  tree on which union will be performed
    * @param other tree on which union will be performed
    * @tparam A data type
    * @return tree which contains all values from both trees
    */
  @tailrec
  def union[A: Ordering](tree: BRTree[A])(other: BRTree[A]): BRTree[A] = {
    tree match {
      case Leaf => other
      case Node(_, value, _, _) =>
        del(value)(tree)
        insert(value)(other)
        union(tree)(other)
    }
  }

  /** Method which returns intersection of two trees.
    * Returned tree is balanced and preserves all black-red constraints
    * @param tree  tree on which intersection will be performed
    * @param other tree on which intersection will be performed
    * @tparam A data type
    * @return tree which contains only those values which are present in both trees
    */
  def intersection[A: Ordering](tree: BRTree[A])(other: BRTree[A]): BRTree[A] = {
    intersect(tree)(other)()
  }

  /**
    * Auxiliary method which calculates intersection of two trees.
    * Returned tree is balanced and preserves all black-red constraints
    *
    * @param tree   tree on which intersection will be performed, common elements are deleted from this tree
    * @param other  tree on which intersection will be performed
    * @param result auxiliary tree, common elements in both trees are added to the result tree
    * @tparam A data type
    * @return tree with elements that exist in both trees
    */
  @tailrec
  private def intersect[A: Ordering](tree: BRTree[A])(other: BRTree[A])(result: BRTree[A] = Leaf): BRTree[A] = {
    tree match {
      case Leaf => result
      case Node(_, value, _, _) =>
        if (contains(other)(value)) {
          del(value)(tree)
          insert(value)(result)
          intersect(tree)(other)(result)
        }
        else {
          del(value)(tree)
          intersect(tree)(other)(result)
        }
    }
  }

  /** Method which throws an exception, when invalid operation is performed
    *
    * @param msg exception message
    * @return new exception
    */
  private def fail(msg: String) = throw new Exception(msg)

  def print[A](tree: BRTree[A]): Unit = {
    tree match {
      case Leaf =>
      case Node(c, v, l, r) => {
        print(l)
        println("color: " + c + " value: " + v + " left:" + l + " right: " + r)
        print(r)
      }
    }
  }

}


object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")

  }
}