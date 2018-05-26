abstract class Color

case object Red extends Color
case object Black extends Color
case object DoubleBlack extends Color
case object NegativeBlack extends Color

sealed abstract class BRTree[+A] {
  def color: Color
}

case object Leaf extends BRTree[Nothing] {
  def color: Color = Black
}

case object LeafDoubleBlack extends BRTree[Nothing] {
  def color: Color = DoubleBlack
}

case class Branch[+A](color: Color, value: A, left: BRTree[A], right: BRTree[A]) extends BRTree[A]

object BRTree {

  def insert[A: Ordering](value: A)(tree: BRTree[A]): BRTree[A] = {
    blacken(ins(value)(tree))
  }

  private def ins[A: Ordering](x: A)(tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Leaf => Branch(Red, x, Leaf, Leaf)
      case Branch(_, value, left, right) =>
        if (Ordering[A].compare(x, value) < 0) balance(Black, x, ins(value)(left), right)
        else if (Ordering[A].compare(x, value) > 0) balance(Black, x, left, ins(value)(right))
        else tree
    }
  }

  private def balance[A](color: Color, value: A, left: BRTree[A], right: BRTree[A]): BRTree[A] =
    (color, value, left, right) match {
      case (Black, z, Branch(Red, y, Branch(Red, x, a, b), c), d) =>
        Branch(Red, y, Branch(Black, x, a, b), Branch(Black, z, c, d))
      case (Black, z, Branch(Red, x, a, Branch(Red, y, b, c)), d) =>
        Branch(Red, y, Branch(Black, x, a, b), Branch(Black, z, c, d))
      case (Black, x, a, Branch(Red, z, Branch(Red, y, b, c), d)) =>
        Branch(Red, y, Branch(Black, x, a, b), Branch(Black, z, c, d))
      case (Black, x, a, Branch(Red, y, b, Branch(Red, z, c, d))) =>
        Branch(Red, y, Branch(Black, x, a, b), Branch(Black, z, c, d))

      case (DoubleBlack, z, Branch(Red, y, Branch(Red, x, a, b), c ), d) =>
        Branch(Red, y, Branch(Black, x, a, b), Branch(Black, z, c, d))
      case (DoubleBlack, z, Branch(Red, x, a, Branch(Red, y, b, c)), d) =>
        Branch(Red, y, Branch(Black, x, a, b), Branch(Black, z, c, d))
      case (DoubleBlack, x, a, Branch(Red, z, Branch(Red, y, b, c), d)) =>
        Branch(Red, y, Branch(Black, x, a, b), Branch(Black, z, c, d))
      case (DoubleBlack, x, a, Branch(Red, y, b, Branch(Red, z, c, d))) =>
        Branch(Red, y, Branch(Black, x, a, b), Branch(Black, z, c, d))
      case (DoubleBlack, x, a, Branch(NegativeBlack, z, Branch(Black, y, b, c), d)) if d.color == Black =>
        Branch(Black, y, Branch(Black, x, a, b), balance(Black, z, c, redden(d)))
      case (DoubleBlack, z, Branch(NegativeBlack, x, a, Branch(Black, y, b, c)), d) if a.color == Black =>
        Branch(Black, y, balance(Black, x, redden(a), b), Branch(Black, z, c, d))

      case (c, x, a, b) => Branch(c, x, a, b)
    }

  private def blacken[A](tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Leaf => tree
      case LeafDoubleBlack => Leaf
      case Branch(_, value, left, right) => Branch(Black, value, left, right)
    }
  }
  private def redden[A](tree: BRTree[A]): BRTree[A] = {
    tree match {
      //case Leaf => error
      //case LeafDoubleBlack => error
      case Branch(_, value, left, right) => Branch(Red, value, left, right)
    }
  }

  private def blacker(color: Color): Color = {
    color match {
      case NegativeBlack => Red
      case Red => Black
      case Black => DoubleBlack
      //case DoubleBlack => error
    }
  }
  private def redder(color: Color): Color = {
    color match {
      //case NegativeBlack => error
      case Red => NegativeBlack
      case Black => Red
      case DoubleBlack => Black
    }
  }

  private def blacker[A](tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Leaf => LeafDoubleBlack
      //case LeafDoubleBlack => error
      case Branch(color, value, left, right) => Branch(blacker(color), value, left, right)
    }
  }

  private def redder[A](tree: BRTree[A]): BRTree[A] = {
    tree match {
      case LeafDoubleBlack => Leaf
      //case Leaf => error
      case Branch(color, value, left, right) => Branch(redder(color), value, left, right)
    }
  }

  def find[A: Ordering](x: A)(tree: BRTree[A]): Boolean = {
    tree match {
      case Leaf => false
      case Branch(_, value, left, right) =>
        if (Ordering[A].compare(x, value) < 0) find(x)(left)
        else if (Ordering[A].compare(x, value) > 0) find(x)(right)
        else true
    }
  }

  def delete[A: Ordering](value: A)(tree: BRTree[A]): BRTree[A] = {
    blacken(del(value)(tree))
  }

  private def del[A: Ordering](value: A)(tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Leaf => Leaf
      case Branch(c, y, l, r) =>
        if (Ordering[A].compare(value, y) < 0) bubble(c, y, del(value)(l), r)
        else if (Ordering[A].compare(value, y) > 0) bubble(c, y, l, del(value)(r))
        else remove(tree)
    }
  }

  private def remove[A: Ordering](tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Leaf => Leaf
      case Branch(Red, _, Leaf, Leaf) => Leaf
      case Branch(Black, _, Leaf, Leaf) => LeafDoubleBlack
      case Branch(Black, _, Leaf, Branch(Red, x, left, right)) => Branch(Black, x, left, right)
      case Branch(Black, _, Branch(Red, x, left, right), Leaf) => Branch(Black, x, left, right)
      case Branch(color, _, left, right) => bubble(color, max(left), removeMax(left), right)
    }
  }

  private def removeMax[A: Ordering](tree: BRTree[A]): BRTree[A] = {
    tree match {
      case Branch(_, _, _, Leaf) => remove(tree)
      case Branch(color, x, left, right) => bubble(color, x, left, removeMax(right))
    }
  }

  private def bubble[A](color: Color, value: A, left: BRTree[A], right: BRTree[A]): BRTree[A] = {
    if (isDoubleBlack(left) || isDoubleBlack(right)) balance(blacker(color), value, redder(left), redder(right))
    else balance(color, value, left, right)
  }


  private def isDoubleBlack[A](tree: BRTree[A]): Boolean = {
    tree match {
      case LeafDoubleBlack => true
      case Branch(DoubleBlack, _, _, _) => true
      case _ => false
    }
  }

  private def max[A](tree: BRTree[A]): A = {
    tree match {
      //case Leaf => error
      case Branch(_, x, _, Leaf) => x
      case Branch(_, _, _, r) => max(r)
    }
  }
}


object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")

  }
}