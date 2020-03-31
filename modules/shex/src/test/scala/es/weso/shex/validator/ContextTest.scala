package es.weso.shex.validator

import org.scalatest._
import matchers.should._
import funspec._
import cats._

class ContextTest extends AnyFunSpec with Matchers with EitherValues {

  describe(s"Context") {
    it("should generate empty context") {
      val ctx = Monoid[Context].empty
      ctx.typing.getMap.isEmpty should be(true)
    }
  }

}
