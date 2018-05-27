import org.scalatest.FunSuite
import BRTree._

class BlackRedTreeTest extends FunSuite {

  test("Leaf is black") {
    val t = Leaf
    assert(t.color == Black)
  }

  test("nodes with the same value are equal") {
    val t1 = Node(value = 6)
    val t2 = Node(value = 6)
    assert(t1 == t2)
  }

  test("nodes with different values are not equal") {
    val t1 = Node(value = 6)
    val t2 = Node(value = 9)
    assert(t1 != t2)
  }

  test("after insertion tree contains inserted node") {
    val t = Leaf
    val t1 = insert(5)(t)
    assert(contains(t1)(5))
  }

    test("when node doesn't exist, tree doesn't contain it") {
    val t = Node(value = 10)
    assert(!contains(t)(5))
  }

  test("after insertion to empty tree, inserted node is black") {
    val t = Leaf
    val t1 = insert(5)(t)
    assert(t1.color == Black)
  }

  test("after adding value which already exists, the same tree is returned") {
    val t = Node (value = 5)
    val t1 = insert(5)(t)
    assert(t == t1)
  }

  test("Height of empty tree is 0") {
    val t = Leaf
    assert(height(t) == 0)
  }

  test("Height of one-node tree is 1") {
    val t = Node(value = 7)
    assert(height(t) == 1)
  }

  test("Height dsss one-node tree is 1") {
    val t = Leaf
    val t1 = insert(5)(t)
    val t2 = insert(6)(t1)
    val t3 = insert(8)(t2)
  print(t3)
    assert(height(t3) == 3)
  }
  test("Height of one-node trcccee is 1") {
    val t = Node(value = 7)
    assert(height(t) == 1)
  }

  test("Height of multiple-nodes tree is correct") {
    val t = Node(Black, 5, Node(Red, 2, Node(value = 1),Leaf), Leaf)
    assert(height(t) == 3)
  }

  test("Height after adding multiple-nodes tree is correct") {
    val t = Node(value = 8)
    val t1 = insert(5)(t)
    val t2 = insert(10)(t1)
    val t3 = insert(9)(t2)
    assert(height(t3) == 3)
  }

  test("After adding 3 elements tree is balanced") {
    val t = Node(value = 1)
    val t1 = insert(5)(t)
    val t2 = insert(10)(t1)
    val t3 = insert(40)(t1)

    assert(height(t2) == 3 ) /////////////////uuuuuuuuuuuuuuuu, nie jest balanced :C ;C ;C
  }

  test("union of two empty trees is empty"){
    val t1 = Leaf
    val t2 = Leaf
    assert(union(t1)(t2) == Leaf)
  }

  test("union of one empty tree with non-empty returns the second one"){
    val t1 = Leaf
    val t2 = Node(value = 7)
    val t3 = insert(4)(t2)
    assert(union(t1)(t3) == t3)
  }












}