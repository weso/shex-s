package es.weso.shex.actions

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.data._
import cats.effect._
import cats.implicits.toShow
import es.weso.utils.IOUtils.fromES

class TestSemanticActionTest extends AnyFunSpec with Matchers with EitherValues {

  describe(s"Test of TestSemanticAction processor") {
    it(s"Should run print code") {
      val rdfStr =
        """|prefix : <http://example.org/>
          |:x :p 1 .
        """.stripMargin
      val r = RDFAsJenaModel.fromString(rdfStr,"TURTLE",None).flatMap(_.use(rdf => for {
        result <- TestSemanticAction.runAction("print(s)", IRI("http://example.org/x"),rdf)
      } yield result))
      r.attempt.unsafeRunSync.fold(
        e => fail(s"Error: $e"),
        result => info(s"Result: $result")
      )
    }
    it(s"Should run print(o) with spaces") {
      val rdfStr =
        """|prefix : <http://example.org/>
           |:x :p 1 .
        """.stripMargin
      val r = RDFAsJenaModel.fromString(rdfStr,"TURTLE",None).flatMap(_.use(rdf => for {
        result <- TestSemanticAction.runAction(" print(o) ", IRI("http://example.org/x"),rdf)
      } yield result))
      r.attempt.unsafeRunSync.fold(
        e => fail(s"Error: $e"),
        result => info(s"Result: $result")
      )
    }
    it(s"Should run fail code") {
      val r = RDFAsJenaModel.empty.flatMap(_.use(rdf => for {
        result <- TestSemanticAction.runAction("fail(s)", IRI(""), rdf)
      } yield result))
      r.attempt.unsafeRunSync.fold(
        e => info(s"Failed as expected: $e"),
        result => fail(s"Should fail but succeeded with result: ${result}")
      )
    }
  }

}
