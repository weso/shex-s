package es.weso.collection

import munit._

import scala.collection.SortedMap

class SortedMapBagTest extends FunSuite {

  test("Should add one element and have multiplicity 1") {
    val sm: SortedMap[Char, Int] = SortedMap[Char, Int]()
    val bag                      = BagSortedMap[Char](sm)
    val expected                 = SortedMap[Char, Int]('b' -> 3)
    assertEquals(bag.add('b', 3).asSortedMap, expected)
  }
}
