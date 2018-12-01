import scala.io.Source

object Calibration {

  def parseNumbers(list: List[String]): List[Int] =
    list.map(change => change.substring(1).toInt * (if (change.charAt(0) == '-') -1 else 1))

  def computeFrequencyPart1(list: List[Int]): Int = list.sum

  def computeFrequencyPart2(list: List[Int]): Int = {
    var currentFreq = 0
    var alreadySetFrequencies = Set(0)
    while (true) {
      val res = list.find(change => {
        currentFreq += change
        val res1 = alreadySetFrequencies.contains(currentFreq)
        if (!res1) {
          alreadySetFrequencies += currentFreq
        }
        res1
      })
      if (res.nonEmpty) {
        return currentFreq
      }
    }
    return -1
  }

  def main(args: Array[String]): Unit = {
    val frequencies = parseNumbers(Source.fromResource("input.txt").getLines().toList)
    val result1 = computeFrequencyPart1(frequencies)
    println(result1)

    val result2 = computeFrequencyPart2(frequencies)
    println(result2)
  }
}
