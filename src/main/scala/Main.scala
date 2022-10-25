import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Main extends App {

  val text = "In 1991, while studying computer science at University of Helsinki, Linus Torvalds began a project that later became the Linux kernel. He wrote the program specifically for the hardware he was using and independent of an operating system because he wanted to use the functions of his new PC with an 80386 processor. Development was done on MINIX using the GNU C Compiler."
  val text2 = "After reading the Linux kernel coding style, you discover the magic of having lines of code with a maximum of 80 characters each. So, you decide that from now on your outgoing emails will also follow a similar pattern and you decide to develop a plugin to help you with that."
  val delimiter = 40 // 15

  println("\n\n")
  run(text, delimiter)
  println("\n\n")
  run(text2, delimiter)
  println("\n\n")

  def run(str: String, delimiter: Int): Unit = {
    listSplit(getSplitPositions(str, delimiter), str).foreach(l => println(l))
  }
  private[this] def getSplitPositions(str: String, delimiter: Int) : List[Int] = {
    val list : ListBuffer[Int] = ListBuffer.empty
    var i = delimiter
    while(i < str.length()) {
      val pos = findNextSplit(i, str)
      list += pos
      i = pos
      i += delimiter
    }
    list.toList
  }
  private[this] def listSplit(pos: List[Int], str: String): List[String] = {
    val (rest, result) = pos.foldRight((str, List[String]())) {
      case (curr, (s, res)) =>
        val (rest, split) = s.splitAt(curr)
        (rest, split.trim :: res)
    }
    rest :: result
  }
  @tailrec private[this] def findNextSplit(pos: Int, str: String): Int = {
      if (pos < 0 || str.charAt(pos) == ' ') pos
      else findNextSplit(pos - 1, str)
  }
}
