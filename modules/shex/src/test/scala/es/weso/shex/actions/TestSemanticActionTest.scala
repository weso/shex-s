package es.weso.shex.actions

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import org.scalatest._
import cats.effect.IO
import cats.data.EitherT

class TestSemanticActionTest extends FunSpec with Matchers with EitherValues {

  describe(s"Test of TestSemanticAction processor") {
    it(s"Should run print code") {
      val rdfStr =
        """|prefix : <http://example.org/>
          |:x :p 1 .
        """.stripMargin
      val r = for {
        rdf <- EitherT.liftF(RDFAsJenaModel.fromString(rdfStr,"TURTLE",None))
        result <- EitherT.fromEither[IO](TestSemanticAction.runAction("print(s)", IRI("http://example.org/x"),rdf))
      } yield result
      r.value.unsafeRunSync.fold(
        e => fail(s"Error: $e"),
        result => info(s"Result: $result")
      )
    }
    it(s"Should run print(o) with spaces") {
      val rdfStr =
        """|prefix : <http://example.org/>
           |:x :p 1 .
        """.stripMargin
      val r = for {
        rdf <- EitherT.liftF(RDFAsJenaModel.fromString(rdfStr,"TURTLE",None))
        result <- EitherT.fromEither[IO](TestSemanticAction.runAction(" print(o) ", IRI("http://example.org/x"),rdf))
      } yield result
      r.value.unsafeRunSync.fold(
        e => fail(s"Error: $e"),
        result => info(s"Result: $result")
      )
    }
    it(s"Should run fail code") {
      val r = for {
        rdf <- EitherT.liftF(RDFAsJenaModel.empty)
        result <- EitherT.fromEither[IO](TestSemanticAction.runAction("fail(s)", IRI(""), rdf))
      } yield result
      r.value.unsafeRunSync.fold(
        e => info(s"Failed as expected: $e"),
        result => fail(s"Should fail but succeeded")
      )
    }
  }

}
