package es.weso.shex.validator

import org.scalatest._
import cats._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ContextTest extends AnyFunSpec with Matchers with EitherValues {

  describe(s"Context") {
    it("should generate empty context") {
      val ctx = Monoid[Context].empty
      ctx.typing.getMap.isEmpty should be(true)
    }
  }

}
