abstract class Color
case object Red extends Color
case object Black extends Color

abstract class BRTree[V] {
  def color: Color
}

case class Leaf[V]() extends BRTree[V] {
  def color: Color = Black
}

case class Branch[V](color: Color, value: V, left: BRTree[V],right: BRTree[V]) extends BRTree[V]


object BRTree {
  import Ordering.Implicits._
  def Add[V:Ordering](x:V, tree: BRTree[V]):BRTree[V] = {
    def BalancedAdd[V:Ordering](tree: BRTree[V], x:V): BRTree[V] =
      tree match  {
      case Leaf() =>  Branch(Red, x, Leaf(), Leaf())
      case Branch(color, value, left, right) =>
        if (x < value)
          Balance(Black, value, BalancedAdd(left, x), right)
        else if (x > value)
          Balance(Black, value, left, BalancedAdd(right,x))
        else tree
      }

    def Balance(color: Color, value: V, left: BRTree[V], right: BRTree[V]): BRTree[V] =
      (color, value, left, right) match {
        case (Black, z, Branch(Red, y, Branch(Red, x, a, b), c), d) =>
          Branch(Red, y, Branch(Black, x, a, b), Branch(Black, z, c, d))
        case (Black, z, Branch(Red, x, a, Branch(Red, y, b, c)), d) =>
          Branch(Red, y, Branch(Black, x, a, b), Branch(Black, z, c, d))
        case (Black, x, a, Branch(Red, z, Branch(Red, y, b, c), d)) =>
          Branch(Red, y, Branch(Black, x, a, b), Branch(Black, z, c, d))
        case (Black, x, a, Branch(Red, y, b, Branch(Red, z, c, d))) =>
          Branch(Red, y, Branch(Black, x, a, b), Branch(Black, z, c, d))
        case (a, b, c, d) => Branch(a, b, c, d)
      }

    def Blacken(tree: BRTree[V]): BRTree[V] =
      tree match {
        case Leaf() => tree
        case Branch(color,value,left,right) => Branch(Black,value,left,right)
      }

    Blacken(BalancedAdd(tree,x))
  }
}
