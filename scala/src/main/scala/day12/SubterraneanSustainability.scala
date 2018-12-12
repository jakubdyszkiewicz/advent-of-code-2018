package day12

import scala.io.Source

object SubterraneanSustainability {

  val initialPad = 20

  private val rulesRegex = raw"([.#]+) => ([.#]{1})".r

  def parseInitialState(line: String): String = line.replace("initial state: ", "")

  type Rules = Map[String, Char]

  def parseRules(lines: Seq[String]): Map[String, Char] =
    lines.map { case rulesRegex(pattern, change) => pattern -> change(0) }.toMap

  /**
    * I wanted to do this in optimal way. It wasn't worth it after all...
    */
  def nextGeneration(pots: String, rules: Rules): String = {
    var window = pots.take(5)
    val sb = new StringBuilder()
    pots.toSeq.drop(5).foreach(pot => {
      window = window.drop(1) + pot
      sb.append(rules.getOrElse(window, '.'))
    })
    pots.take(3) + sb.toString + "..."
  }

  def countPlants(pots: String): Int = {
    var result = 0
    for (i <- 0 until pots.length) {
      if (pots(i) == '#') {
        result += i - initialPad
      }
    }
    result
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day12/input.txt").getLines.toSeq
    val rules = parseRules(input.drop(2))
    var state = ("." * initialPad) + parseInitialState(input.head) + "..."
    println(0 + " " + state)
    for (i <- 1 to 100) {
      state = nextGeneration(state, rules)
      println(i + " " + state)
    }
    println(countPlants(state))
  }
}
