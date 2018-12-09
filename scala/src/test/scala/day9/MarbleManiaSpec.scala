package day9

import org.scalatest.{FunSuite, Matchers}

class MarbleManiaSpec extends FunSuite with Matchers {

  test("should pass for sample data") {
    MarbleMania.highScore(nPlayers = 9, pointsLimit = 25) shouldBe 32
  }

  test("should pass more sample data") {
//    MarbleMania.highScore(nPlayers = 10, pointsLimit = 1618) shouldBe 8317
//    MarbleMania.highScore(nPlayers = 13, pointsLimit = 7999) shouldBe 146373
    MarbleMania.highScore(nPlayers = 17, pointsLimit = 1104) shouldBe 2764

  }

}
