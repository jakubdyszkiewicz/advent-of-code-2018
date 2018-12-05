package day5

import org.scalatest.{FlatSpec, FunSuite, Matchers}

class AlchemicalReductionSpec extends FunSuite with Matchers {

  test("Should reduce polymers") {
    AlchemicalReduction.reducePolymers("dabAcCaCBAcCcaDA") shouldEqual "dabCBAcaDA".toSeq
  }

  test("Should choose the kind of polymer to best reduce polymers") {
    AlchemicalReduction.bestReduceWithoutOnePolymerKind("dabAcCaCBAcCcaDA").length shouldEqual "daDA".length
  }
}
