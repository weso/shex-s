package es.weso.rbe

import es.weso.collection._
import munit._

class RbeTest extends FunSuite {

  test("Symbols") {
    val rbe = Or(And(Symbol("a", 1, 3), Symbol("b", 1, 1)), Symbol("b", 2, 3))
    assertEquals(rbe.symbols.toSet, Set("a", "b"))
  }

  test("No symbols in bag") {
    val rbe = Or(And(Symbol("a", 1, 3), Symbol("b", 1, 1)), Symbol("b", 2, 3))
    assertEquals(rbe.noSymbolsInBag(Bag("a", "c")), false)
  }

}
