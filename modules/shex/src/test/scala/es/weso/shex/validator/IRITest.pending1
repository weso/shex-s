package es.weso.shex.validator

import org.scalatest._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shex.Schema
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class IRITest extends AnyFunSpec with Matchers with EitherValues {
  val rdf = RDFAsJenaModel.empty

  describe(s"Test IRI") {
    it(s"Resolves basic") {
      val r = RDFAsJenaModel.fromChars("""|<x> <p> 1""".stripMargin, "TURTLE", Some(IRI("http://example.org/"))).flatMap(_.use(rdf =>
        for {
         schema <- Schema.fromString("""|<S> { <p> . }""".stripMargin, "ShExC", Some(IRI("http://example.org/")))
         ts <- rdf.triplesWithSubject(IRI("http://example.org/x")).compile.toList
        } yield ts))
      r.attempt.unsafeRunSync.fold(e => fail(s"Error $e"), values => {
        val ts = values
        ts.size should be(1)
      })
    }
  }
}
