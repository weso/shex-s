package es.weso.shex
import org.scalatest._
import cats.effect.IO
import cats.data.EitherT
import matchers.should._
import funspec._


class WellFormedTest extends AnyFunSpec with Matchers with EitherValues {

  describe(s"WellFormedTest") {

/*    shouldCheckWellFormed("""|prefix : <http://example.org/>
      |:S { :p . }
      |""".stripMargin, "example1")

    shouldCheckWellFormed("""|prefix : <http://example.org/>
      |:s { $:lbl (&:lbl ; :p . ) }
      |""".stripMargin, "recursive lbl")
*/
    shouldCheckNotWellFormed("""|prefix : <http://example.org/>
      |:s &:t { }
      |""".stripMargin, "shape :s depends on shape :t that doesn't exist")

  }

  def shouldCheckWellFormed(strSchema: String, label: String): Unit = {
    it(s"Should check well formed $label") {
      (for {
        schema <- EitherT.liftF(Schema.fromString(strSchema))
        _      <- EitherT.fromEither[IO](schema.wellFormed)
      } yield ()).value.unsafeRunSync.fold(e => fail(s"Not well formed $label: $e"), _ => info(s"well formed $label"))
    }
  }

  def shouldCheckNotWellFormed(strSchema: String, label: String): Unit = {
    it(s"Should check not well formed $label") {
      (for {
        schema <- EitherT.liftF(Schema.fromString(strSchema))
        _      <- EitherT.fromEither[IO](schema.wellFormed)
      } yield ()).fold(
        e => info(s"Not well formed $label with error $e as expected"),
        _ => fail(s"Should not be well formed $label. Input:\n$strSchema")
      )
    }
  }

}
