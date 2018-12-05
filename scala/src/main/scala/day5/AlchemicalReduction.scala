package day5

import scala.io.Source

object AlchemicalReduction {

  type Polymers = Seq[Char]
  def reducePolymers(polymers: Polymers, survived: List[Char] = List()): Polymers = {
    if (polymers.isEmpty) {
      return survived
    }
    val sameLetterDifferentCase = (x: Char, y: Char) => (x - y).abs == 32
    if (survived.nonEmpty && sameLetterDifferentCase(polymers.head, survived.head)) {
      reducePolymers(polymers.tail, survived.tail)
    }
    else {
      reducePolymers(polymers.tail, polymers.head :: survived)
    }
  }

  def bestReduceWithoutOnePolymerKind(polymers: Polymers): Polymers = {
    ('a' to 'z')
      .map(letter => polymers.filter(polymer => polymer != letter && polymer != letter.toUpper))
      .map(polymers => reducePolymers(polymers))
      .minBy(polymers => polymers.length)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day5/input.txt").getLines.next
    println(reducePolymers(input).length)
    println(bestReduceWithoutOnePolymerKind(input).length)
  }
}
