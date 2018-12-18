package day18

import commons.Crossable.Crossable

import scala.io.Source

object SettlersOfTheNorthPole {

  type Field = Array[Array[Char]]
  def newField(dim: Int): Field = Array.ofDim(dim, dim)

  private val deltas = (0, -1) :: (0, 1) :: (1, -1) :: (1, 0) :: (1, 1) :: (-1, -1) :: (-1, 0) :: (-1, 1) :: Nil
  def adjacents(field: Field, x: Int, y: Int): Seq[Char] =
    deltas
      .map { case(dX, dY) => (x + dX, y + dY) }
      .filter { case(x, y) => x >= 0 && x < field.length && y >= 0 && y < field.length }
      .map { case(x, y) => field(y)(x) }

  def nextState(field: Field): Field = {
    val nextState = newField(field.length)
    field.indices.cross(field.indices)
      .foreach { case(x, y) =>
        val adjs = adjacents(field, x, y)
        val adjsTrees = adjs.count(_ == '|')
        val adjsLumberyard = adjs.count(_ == '#')
        nextState(y)(x) = field(y)(x) match {
          case '.' => if (adjsTrees >= 3) '|' else '.'
          case '|' => if (adjsLumberyard >= 3) '#' else '|'
          case '#' => if (adjsTrees >= 1 && adjsLumberyard >= 1) '#' else '.'
        }
      }
    nextState
  }

  def countFields(field: Field): Map[Char, Int] =
    field.flatten.groupBy(identity).mapValues(_.length)

  def printField(field: Field): Unit = {
    for (i <- field.indices) {
      for (j <- field(i).indices) {
        print(field(i)(j))
      }
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    val field = Source.fromResource("day18/input.txt").getLines
      .filterNot(_.isEmpty)
      .map(_.toCharArray)
      .toArray

    val fieldAfter10Steps = (0 until 10).foldLeft(field) { case(field, _) => nextState(field) }
    val fieldsByType = countFields(fieldAfter10Steps)
    println(fieldsByType('|') * fieldsByType('#'))

    (0 until 1000).foldLeft(field) { case(field, iter) =>
      val fieldsByType = countFields(field)
      val res = fieldsByType('|') * fieldsByType('#')
      println(iter + " " + res)
      nextState(field)
      // cycle starts on 537 and is same every 28 element
    }
  }
}
