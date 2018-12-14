package day13

import org.scalatest.{FunSuite, Matchers}
import MineCartMadness._

class MineCartMadnessSpec extends FunSuite with Matchers {

  test("should move cart >- to ->") {
    tickSimulation(">-") shouldBe Cart(x = 1, y = 0, Right)
  }

  private def tickSimulation(lines: String*): Cart = {
    val (field, carts) = parseFieldsAndCarts(lines)
    tick(field, carts).head
  }

  test("should move cart -< to <-") {
    tickSimulation("-<") shouldBe Cart(x = 0, y = 0, Left)
  }

  test("should move cart down") {
    tickSimulation("v", "|") shouldBe Cart(x = 0, y = 1, Down)
  }

  test("should move cart up") {
    tickSimulation("|", "^") shouldBe Cart(x = 0, y = 0, Up)
  }

  test("should turn down on the right corner") {
    tickSimulation(">\\") shouldBe Cart(x = 1, y = 0, Down)
  }

  test("should turn up on the right corner") {
    tickSimulation(">/") shouldBe Cart(x = 1, y = 0, Up)
  }

  test("should turn up on the left corner") {
    tickSimulation("""\<""") shouldBe Cart(x = 0, y = 0, Up)
  }

  test("should turn down on the left corner") {
    tickSimulation("""/<""") shouldBe Cart(x = 0, y = 0, Down)
  }

}
