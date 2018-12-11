package day6

import java.util.concurrent.atomic.AtomicInteger

import scala.io.Source
import commons.Crossable.Crossable

object ChronalCoordinates {

  case class Coord(id: Int, x: Int, y: Int)

  object Coord {
    def apply(x: Int, y: Int): Coord = new Coord(Coord.idGenerator.incrementAndGet, x, y)
    val idGenerator = new AtomicInteger(0)
  }

  case class Field(startX: Int, endX: Int, startY: Int, endY: Int) {

  }

  def stepsBetween(coord1: Coord, coord2: Coord): Int =
    (coord1.x - coord2.x).abs + (coord1.y - coord2.y).abs

  def closest(coord: Coord, coords: Seq[Coord]): Seq[Coord] = {
    coords.map(c => c -> stepsBetween(c, coord))
      .groupBy(_._2)
      .minBy(_._1)
      ._2
      .map(_._1)
  }

  def largestNotInfiniteArea(coords: Seq[Coord]): (Int, Traversable[Coord]) = {
    val minX = coords.map(_.x).min
    val maxX = coords.map(_.x).max
    val minY = coords.map(_.y).min
    val maxY = coords.map(_.y).max

    def isCoordsInfinite(coords: Coord) =
      ((coords.x == minX || coords.x == maxX) && (minY to maxY).contains(coords.y)) ||
      ((coords.y == minY || coords.y == maxY) && (minX to maxX).contains(coords.x))

    (minX to maxX).cross(minY to maxY)
      .map { case(x, y) => Coord(-1, x, y)  }
      .map(coord => coord -> closest(coord, coords))
      .filter { case(coord, closest) => closest.length == 1 }
      .groupBy { case(coord, closest) => closest.head.id }
      .mapValues(coordsWithClosests => coordsWithClosests.map(_._1))
      .filter { case(id, coordsForId) => coordsForId.forall(c => !isCoordsInfinite(c)) }
      .maxBy { case(id, coordsForId) => coordsForId.size }
  }

  def part2(coords: Seq[Coord], regionLimit: Int): Int = {
    val minX = coords.map(_.x).min
    val maxX = coords.map(_.x).max
    val minY = coords.map(_.y).min
    val maxY = coords.map(_.y).max

    (minX to maxX).cross(minY to maxY)
      .map { case(x, y) => Coord(-1, x, y)  }
      .map(c1 =>
        coords
          .map(c2 => stepsBetween(c1, c2))
          .sum
      )
      .count(_ < regionLimit)
  }

  def main(args: Array[String]): Unit = {
    val coords = Source.fromResource("day6/input.txt").getLines
      .map(x => x.split(", "))
      .map { case Array(x, y) => Coord(x.toInt, y.toInt) }
      .toSeq

    val result = largestNotInfiniteArea(coords)
    println(result._2.size)
    println(part2(coords, 10000))
  }
}
