import scala.io.Source

object Inventory {

  def checksum(boxes: Seq[String]): Int = {
    val countedLetterBoxes = boxes.map(box => box.toSeq.groupBy(letter => letter).mapValues(_.size))
    def sameLetterBoxes(occurrence: Int) = countedLetterBoxes.count(box => box.values.exists(numOfLetters => numOfLetters == occurrence))
    sameLetterBoxes(2) * sameLetterBoxes(3)
  }

  /** O(n^2) solution */
  def commonLettersChecksum(boxes: Seq[String]): Seq[Char] = {

    def commonLetters(box1: String, box2: String): Seq[Char] = box1.toSeq.intersect(box2.toSeq)

    def isDifferentByOneLetterOnSamePosition(box1: String, box2: String): Boolean =
      box1.toSeq.zip(box2.toSeq).count(x => x._1 != x._2) == 1

    val box1 = boxes.find(box => boxes.exists(x => isDifferentByOneLetterOnSamePosition(box, x))).get
    val box2 = boxes.find(x => isDifferentByOneLetterOnSamePosition(x, box1)).get
    commonLetters(box1, box2)
  }

  /** O(number of words * (letters in word)^2) */
  def commonLettersChecksum2(boxes: Seq[String]): Option[String] = {

    def boxesWithoutLetterAtIndex(index: Int): Seq[String] =
      boxes.map(box => box.take(index - 1) + box.drop(index))

    def findDuplicate(boxes: Seq[String], visitedBoxes: Set[String] = Set()): Option[String] =
      if (boxes.isEmpty) None
      else if (visitedBoxes.contains(boxes.head)) Some(boxes.head)
      else findDuplicate(boxes.tail, visitedBoxes + boxes.head)

    Range(1, boxes.head.length)
      .map(boxesWithoutLetterAtIndex)
      .map(findDuplicate(_))
      .find(_.nonEmpty)
      .flatten
  }

  def main(args: Array[String]): Unit = {
    val boxes = Source.fromResource("day2input.txt").getLines.toSeq
    println(checksum(boxes))

    val sampleBoxes = List("abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
    println(checksum(sampleBoxes))

    val part2Sample = List("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")
    println(commonLettersChecksum(part2Sample))

    println(commonLettersChecksum(boxes))
    println(commonLettersChecksum2(boxes).get)
  }
}
