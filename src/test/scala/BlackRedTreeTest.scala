import org.scalatest.FunSuite
import RBTree._

class BlackRedTreeTest extends FunSuite {

  def testTree = Node(Black,5,Node(Black,3,Node(Black,2,Node(Red,1,Leaf,Leaf),Leaf),Node(Black,4,Leaf,Leaf)),
                Node(Black,7,Node(Black,6,Leaf,Leaf),Node(Red,9,Node(Black,8,Leaf,Leaf),Node(Black,10,Leaf,Leaf))))

  test("Leaf is black") {
    val t = Leaf
    assert(t.color == Black)
    assert(isTreeBlackRed(t))
  }

  test("after insertion tree contains inserted node") {
    val t = Leaf
    val t1 = insert(5)(t)
    assert(contains(t1)(5))
    assert(isTreeBlackRed(t))
  }

  test("when node doesn't exist, tree doesn't contain it") {
    val t = Node(value = 10)
    assert(!contains(t)(5))
  }

  test("after insertion to empty tree, inserted node is black") {
    val t = Leaf
    val t1 = insert(5)(t)
    assert(t1.color == Black)
    assert(isTreeBlackRed(t))
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

  test("after insertion of 3 nodes tree is still red-black ") {
    val t = Leaf
    val t1 = insert(5)(t)
    val t2 = insert(6)(t1)
    val t3 = insert(8)(t2)
    assert(isTreeBlackRed(t3))
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
    assert(isTreeBlackRed(t3))
  }

 test("union of two non-empty trees is performed correctly"){
     val t = testTree
     val t1 = delete(8)(t)
     val t2 = testTree
     val t3 = delete(10)(t2)
     assert(union(t1)(t3) == testTree)
     assert(isTreeBlackRed(t3))
   }

 test("union is symetric"){
    val t = testTree
    val t1 = delete(8)(t)
    val t2 = testTree
    val t3 = delete(10)(t2)
    assert(union(t1)(t3) == union(t3)(t1))
  }


  test("intersection of two empty trees is empty"){
    val t1 = Leaf
    val t2 = Leaf
    assert(intersection(t1)(t2) == Leaf)
  }

  test("intersection of one empty tree with non-empty is empty"){
    val t1 = Leaf
    val t2 = Node(value = 7)
    assert(intersection(t1)(t2) == Leaf)
  }

  test("intersection of equal trees returns that tree") {
    val t1 =  Node(value = 7)
    val t2 =  Node(value = 7)
    assert(intersection(t2)(t1) == Node(value = 7))
  }

 test("intersection is symetric") {
    val t1 =  testTree
    val t2 =  testTree
    assert(intersection(t2)(t1) == intersection(t1)(t2))
 }

  test("intersection of two different trees performs correctly") {
    val t1 = testTree
    val t2 = Node(Black,5,Node(Black,0,Node(Black,-1,Leaf,Leaf),Node(Black,3,Leaf,Leaf)),
           Node(Black,20,Node(Black,10,Leaf,Leaf),Node(Black,22,Leaf,Leaf)))
    val t3 = intersection(t1)(t2)
    assert( t3 == Node(Black,5,Node(Red,3,Leaf,Leaf),Node(Red,10,Leaf,Leaf)))
    assert(isTreeBlackRed(t3))

  }
  test("after deletion of the only-node tree is empty"){
    val t = Node(value = 1)
    val t1 = delete(1)(t)
    assert(t1 == Leaf)
  }

  test("after deletion of non-existing node exception is thrown"){
    val t = Node(Black,5,Node(Red,3,Leaf,Leaf),Leaf)
    assertThrows[Exception](delete(7)(t))
  }

  test("after deletion of node with no children tree is correct"){
    val t = testTree
    val t1 = delete(7)(t)
    assert(!contains(t1)(7))
    assert(isTreeBlackRed(t1))
  }

  test("after deletion of node with only one child tree is correct"){
    val t = testTree
    val t1 = delete(2)(t)
    assert(!contains(t1)(2))
    assert(isTreeBlackRed(t1))
  }

    test("after deletion of node with two children tree is correct"){
    val t = testTree
    val t1 = delete(3)(t)
    assert(!contains(t1)(3))
    assert(isTreeBlackRed(t1))
  }
}
