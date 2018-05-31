import scala.annotation.tailrec
import scala.io.StdIn
import RBTree._
object Main extends App {
  private var testTree:RBTree[Int]= Leaf

  private val number = "0|-?[1-9]\\d*"
  private val addValue = raw"""\s*add\s*($number)\s*""".r
  private val deleteValue = raw"""\s*delete\s*($number)\s*""".r
  private val containsValue = raw"""\s*contains\s*($number)\s*""".r
  private val printTree = """\s*print\s*""".r
  private val help = """\s*help\s*""".r
  private val exit = """\s*exit\s*""".r

  private def printUsage: Unit = {
    println("Commands:\n" +
      "add [Int] - add value to RBTree\n" +
      "delete [Int] - remove value from RBTree\n" +
      "exists [Int] - check if tree contains value\n" +
      "print - print tree\n" +
      "help - print program usage\n" +
      "exit - exit program")
  }

  @tailrec
  final def main: Unit = {
  print("$")
    StdIn.readLine match {
      case addValue(v) => {
        val value = v.toInt
        if (!contains(testTree)(value)) {
          testTree = insert(value)(testTree)
          println("tree after insertion: " + testTree)
        }
        else
          println("Value " + value + " already exists")
        main
      }
      case deleteValue(v) => {
        val value = v.toInt
        if (contains(testTree)(value)) {
           testTree = delete(value)(testTree)
          println("tree after deletion: " + testTree)

        }
        else
           println("Value " + value + " does not exist")
        main
      }
      case containsValue(value) => {
        println(contains(testTree)(value.toInt))
        main
      }
      case printTree(_*) => {
        println(testTree)
        main
      }
      case help(_*) => {
        printUsage
        main
      }
      case exit(_*) => println("Exiting...")
      case _ => println("Incorrect input"); main
    }
  }

  printUsage
  main
}