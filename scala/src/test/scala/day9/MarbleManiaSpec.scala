package day9

import org.scalatest.{FunSuite, Matchers}

class MarbleManiaSpec extends FunSuite with Matchers {

  test("should pass for sample data") {
    MarbleMania.highScore(nPlayers = 9, pointsLimit = 25) shouldBe 32
  }
}
