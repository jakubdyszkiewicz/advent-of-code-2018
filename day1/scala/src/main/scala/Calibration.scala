import scala.annotation.tailrec
import scala.io.Source

object Calibration {

  def parseNumbers(list: List[String]): List[Int] = list.map(removePlusSign).map(_.toInt)
  private def removePlusSign(num: String) = if (num startsWith "+") num.substring(1) else num

  def computeFrequencyPart1(list: List[Int]): Int = list.sum

  def computeFrequencyPart2(list: List[Int])(currentFreq: Int = 0, visitedFreqs: Set[Int] = Set()): Int = {

    @tailrec def computeFrequencyRec(list: List[Int], currentFreq: Int, visitedFreqs: Set[Int]): (Int, Set[Int]) = {
      if (list.isEmpty) {
        return (currentFreq, visitedFreqs)
      }
      val nextFreq = currentFreq + list.head
      if (visitedFreqs contains nextFreq) (nextFreq, visitedFreqs)
      else computeFrequencyRec(list.tail, nextFreq, visitedFreqs + nextFreq)
    }

    if (visitedFreqs contains currentFreq) currentFreq
    else computeFrequencyPart2(list).tupled(computeFrequencyRec(list, currentFreq, visitedFreqs))
  }

  def main(args: Array[String]): Unit = {
    val frequencies = parseNumbers(Source.fromResource("input.txt").getLines().toList)

    val result1 = computeFrequencyPart1(frequencies)
    println(result1)

    val result2 = computeFrequencyPart2(frequencies)
    println(result2)
  }
}
