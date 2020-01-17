package es.weso.collection

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SortedMap

class SortedMapBagTest extends AnyFunSpec with Matchers {

  describe("A SortedMap Bag") {
    it("Should add one element and have multiplicity 1") {
      val sm: SortedMap[Char, Int] = SortedMap[Char, Int]()
      val bag                      = BagSortedMap[Char](sm)
      val expected                 = SortedMap[Char, Int]('b' -> 3)
      bag.add('b', 3).asSortedMap should be(expected)
    }
  }
}
