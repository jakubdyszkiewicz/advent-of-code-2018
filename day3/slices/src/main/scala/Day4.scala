import scala.io.Source

object Day4 {

  def eliminate(input: Seq[Char], survived: List[Char] = List()): List[Char] = {
    if (input.isEmpty) {
      return survived
    }
    val sameLetterDifferentCase = (x: Char, y: Char) => (x - y).abs == 32
    if (survived.nonEmpty && sameLetterDifferentCase(input.head, survived.head)) eliminate(input.tail, survived.tail)
    else eliminate(input.tail, input.head :: survived)
  }

  def eliminateOneLetter(input: Seq[Char]): Int = {
    ('a' to 'z')
      .map(letter => input.filter(x => x != letter && x != letter.toUpper))
      .map(seq => eliminate(seq).length)
      .min
  }


  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("inputDay5.txt").getLines.next
    val sample = "dabAcCaCBAcCcaDA"

    println(eliminate(sample).length)
    println(eliminate(input).length)

    println(eliminateOneLetter(sample))
    println(eliminateOneLetter(input))
  }
}
