package day8

import org.scalatest.{FunSuite, Matchers}

class MemoryManeuverSpec extends FunSuite with Matchers {

  test("should pass test data") {
    val input = List(2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2)

    MemoryManeuver.part1(input) shouldEqual 138
  }

  test("should pass test data for part 2") {
    val input = List(2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2)

    MemoryManeuver.part2(input) shouldEqual 66
  }

  test("should solve for tree without children") {
    // given
    val input = List(0, 3, 1, 2, 3)

    // when
    val (_, metadata) = MemoryManeuver.extractMetadata(input)

    // then
    metadata should contain theSameElementsAs List(1, 2, 3)
  }

  test("should solve for tree with one child") {
    // given
    val input = List(1, 2, 0, 2, 1, 2, 3, 4)

    // when
    val (_, metadata) = MemoryManeuver.extractMetadata(input)

    // then
    metadata should contain theSameElementsAs List(1, 2, 3, 4)
  }

  test("should solve for tree with subtree with subtree") {
    // given
    val input = List(1, 1, 1, 1, 0, 1, 1, 2, 3)

    // when
    val (_, metadata) = MemoryManeuver.extractMetadata(input)

    // then
    metadata should contain theSameElementsAs List(1, 2, 3)
  }

  test("should solve step") {
    // given
    val input = List(1, 1, 0, 1, 1, 2, 3)

    // when
    val (_, metadata) = MemoryManeuver.extractMetadata(input)

    // then
    metadata should contain theSameElementsAs List(1, 2)
  }

  test("should solve for two subtrees") {
    // given
    val input = List(2, 1, 0, 1, 1, 0, 1, 2, 3)

    // when
    val (_, metadata) = MemoryManeuver.extractMetadata(input)

    // then
    metadata should contain theSameElementsAs List(1, 2, 3)
  }
}
