package es.weso.shex.validator

import org.scalatest._
<<<<<<< HEAD
import cats._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
=======
import matchers.should._
import funspec._
import cats._
>>>>>>> issue57

class ContextTest extends AnyFunSpec with Matchers with EitherValues {

  describe(s"Context") {
    it("should generate empty context") {
      val ctx = Monoid[Context].empty
      ctx.typing.getMap.isEmpty should be(true)
    }
  }

}
