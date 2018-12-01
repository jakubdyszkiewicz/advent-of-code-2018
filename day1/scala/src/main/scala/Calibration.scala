import scala.annotation.tailrec
import scala.io.Source

object Calibration {

  def parseNumbers(list: List[String]): List[Int] = list.map(removePlusSign).map(_.toInt)
  private def removePlusSign(num: String) = if (num startsWith "+") num.substring(1) else num

  def computeFrequencyPart1(changes: List[Int]): Int = changes.sum

  def computeFrequencyPart2(changes: List[Int]): Int = {

    @tailrec def visitFrequencies(changes: Stream[Int], currentFreq: Int, visitedFreqs: Set[Int]): Int = {
      val nextFreq = currentFreq + changes.head
      if (visitedFreqs contains nextFreq) nextFreq
      else visitFrequencies(changes.tail, nextFreq, visitedFreqs + nextFreq)
    }

    val repeatingChanges = Stream.continually(changes.toStream).flatten
    visitFrequencies(repeatingChanges, currentFreq = 0, visitedFreqs = Set(0))
  }

  def main(args: Array[String]): Unit = {
    val changes = parseNumbers(Source.fromResource("input.txt").getLines().toList)

    val result1 = computeFrequencyPart1(changes)
    println(result1)

    val result2 = computeFrequencyPart2(changes)
    println(result2)
  }
}
