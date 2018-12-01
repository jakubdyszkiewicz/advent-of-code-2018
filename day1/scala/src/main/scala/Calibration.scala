import scala.annotation.tailrec
import scala.io.Source

object Calibration {

  def parseNumbers(list: List[String]): List[Int] =
    list.map(change => change.substring(1).toInt * (if (change.charAt(0) == '-') -1 else 1))

  def computeFrequencyPart1(list: List[Int]): Int = list.sum

  def computeFrequencyPart2(list: List[Int]): Int = {

    @tailrec def computeFrequencyRec(list: List[Int], currentFreq: Int, visitedFreqs: Set[Int]): (Int, Set[Int]) = {
      if (list.isEmpty) {
        return (currentFreq, visitedFreqs)
      }
      val nextFreq = currentFreq + list.head
      if (visitedFreqs contains nextFreq) (nextFreq, visitedFreqs)
      else computeFrequencyRec(list.tail, nextFreq, visitedFreqs + nextFreq)
    }

    val currentFreq = 0
    val visitedFreqs = Set(0)
    do {
      (currentFreq, visitedFreqs) = computeFrequencyRec(list, currentFreq, visitedFreqs)
    } while (visitedFreqs contains currentFreq)
    currentFreq
  }

  def main(args: Array[String]): Unit = {
    val frequencies = parseNumbers(Source.fromResource("input.txt").getLines().toList)
    val result1 = computeFrequencyPart1(frequencies)
    println(result1)

    val result2 = computeFrequencyPart2(frequencies)
    println(result2)
  }
}
