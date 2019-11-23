package es.weso.shex
import org.scalatest._

class WellFormedTest extends FunSpec with Matchers with EitherValues {

  describe(s"WellFormedTest") {

/*  shouldCheckWellFormed(
   """|prefix : <http://example.org/>
      |:S { :p . }
      |""".stripMargin
      , "example1")
*/
  shouldCheckWellFormed(
   """|prefix : <http://example.org/>
      |:s { $:lbl (&:lbl ; :p . ) }
      |""".stripMargin
      , "recursive lbl")

  }

  def shouldCheckWellFormed(strSchema: String, label: String): Unit = {
    it(s"Should check well formed $label") {
      (for {
        schema <- Schema.fromString(strSchema)
        _ <- schema.wellFormed
      } yield ()).fold(e => fail(s"Not well formed $label: $e"),
        _ => info(s"well formed $label") 
      )
    }
  }
}
