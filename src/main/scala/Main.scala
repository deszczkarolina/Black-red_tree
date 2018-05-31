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

  private def printHelp: Unit = {
    println("Commands:\n" +
      "add [Int] - add value to RBTree\n" +
      "delete [Int] - remove value from RBTree\n" +
      "exists [Int] - check if tree contains value\n" +
      "print - print tree\n" +
      "help - print program usage\n" +
      "exit - exit program")
  }

  @tailrec
  final def mainLoop: Unit = {

    StdIn.readLine match {
      case addValue(v) => {
        val value = v.toInt
        if (!contains(testTree)(value)) {
          println("Adding " + value)
          testTree = insert(value)(testTree)
        }
        else
          println("Value " + value + " already exists")
        mainLoop
      }
      case deleteValue(v) => {
        val value = v.toInt
        if (contains(testTree)(value)) {
          println("Removing " + value)
          testTree = delete(value)(testTree)
        }
        else
           println("Value " + value + " does not exist")
        mainLoop
      }
      case containsValue(value) => {
        println(contains(testTree)(value.toInt))
        mainLoop
      }
      case printTree(_*) => {
        println("Printing...")
        println(testTree)
        mainLoop
      }
      case help(_*) => {
        printHelp
        mainLoop
      }
      case exit(_*) => println("Exiting...")
      case _ => println("Incorrect input"); mainLoop
    }
  }

  printHelp
  mainLoop
}