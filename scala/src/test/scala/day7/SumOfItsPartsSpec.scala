package day7

import org.scalatest.{FunSuite, Matchers}

class SumOfItsPartsSpec extends FunSuite with Matchers {

  test("parse dependency") {
    val (from, to) = SumOfItsParts.parseDependency("Step C must be finished before step A can begin.")
    from shouldEqual 'C'
    to shouldEqual 'A'
  }

  test("should travel for test data") {
    val testData = List(
      ('C', 'A'),
      ('C', 'F'),
      ('A', 'B'),
      ('A', 'D'),
      ('B', 'E'),
      ('D', 'E'),
      ('F', 'E')
    )

    val graph = SumOfItsParts.buildGraph(testData)
    val chars = SumOfItsParts.solution1(graph)
    println(chars)
  }
}
