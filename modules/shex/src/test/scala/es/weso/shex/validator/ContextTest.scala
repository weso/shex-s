package es.weso.shex.validator

import munit._
import cats._

class ContextTest extends FunSuite {

  
 test("should generate empty context") {
      val ctx = Monoid[Context].empty
  assertEquals(ctx.typing.getMap.isEmpty, true)
 }

}
