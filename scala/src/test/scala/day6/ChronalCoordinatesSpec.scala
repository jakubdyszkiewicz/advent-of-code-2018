package day6

import day6.ChronalCoordinates.Coord
import org.scalatest.{FunSuite, Matchers}

class ChronalCoordinatesSpec extends FunSuite with Matchers {

  val coords = List(
    Coord(1, 1),
    Coord(1, 6),
    Coord(8, 3),
    Coord(3, 4),
    Coord(5, 5),
    Coord(8, 9),
  )

  test("should pass example test") {

    val (id, coordsForId) = ChronalCoordinates.largestNotInfiniteArea(coords)

    coordsForId.size shouldEqual 17
    id shouldEqual 5
  }

  test("part2") {
    ChronalCoordinates.part2(coords, 32) shouldEqual 16
  }

}
