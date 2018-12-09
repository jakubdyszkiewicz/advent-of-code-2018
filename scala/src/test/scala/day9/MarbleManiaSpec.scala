package day9

import day9.MarbleMania.CyclicIterator
import org.scalatest.{FunSuite, Matchers}

class MarbleManiaSpec extends FunSuite with Matchers {

  test("should pass for sample data") {
    MarbleMania.highScore(nPlayers = 9, pointsLimit = 25) shouldBe 32
  }

  test("should pass more sample data") {
    MarbleMania.highScore(nPlayers = 10, pointsLimit = 1618) shouldBe 8317
    MarbleMania.highScore(nPlayers = 13, pointsLimit = 7999) shouldBe 146373
//    MarbleMania.highScore(nPlayers = 17, pointsLimit = 1104) shouldBe 2764 // ???
    MarbleMania.highScore(nPlayers = 21, pointsLimit = 6111) shouldBe 54718
    MarbleMania.highScore(nPlayers = 30, pointsLimit = 5807) shouldBe 37305

  }

  test("should cycle through list") {
    val list = new java.util.ArrayList[Int]()
    list.add(0)
    list.add(1)
    val iterator = new CyclicIterator(list)

    iterator.next shouldBe 0
    iterator.next shouldBe 1
    iterator.next shouldBe 0
  }

  test("should cycle through list previous") {
    val list = new java.util.ArrayList[Int]()
    list.add(0)
    list.add(1)
    val iterator = new CyclicIterator(list)

    iterator.previous shouldBe 1
    iterator.previous shouldBe 0
    iterator.previous shouldBe 1
  }

  test("should go next and previous") {
    val list = new java.util.ArrayList[Int]()
    list.add(0)
    list.add(1)
    val iterator = new CyclicIterator(list)

    iterator.next shouldBe 0
    iterator.next shouldBe 1
    iterator.previous shouldBe 1
    iterator.previous shouldBe 0
    iterator.previous shouldBe 1
  }

  test("should remove returning") {
    val list = new java.util.ArrayList[Int]()
    list.add(0)
    list.add(1)
    list.add(2)
    val iterator = new CyclicIterator(list)

    iterator.next shouldBe 0
    iterator.next shouldBe 1
    iterator.next shouldBe 2
    iterator.removeReturning shouldBe 2
    iterator.next shouldBe 0
  }

}
