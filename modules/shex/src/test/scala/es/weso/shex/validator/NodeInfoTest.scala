package es.weso.shex.validator

import es.weso.rdf.nodes._
import org.scalatest._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.jena.RDFAsJenaModel
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import es.weso.rdf.RDFReader
import cats.effect.IO
// import cats.data._
// import es.weso.utils.IOUtils._

class NodeInfoTest extends AnyFunSpec with Matchers with EitherValues {
  // val rdf = RDFAsJenaModel.empty

  describe("totalDigits") {
    it("Should calculate total digits of 3.14") {
      checkOK(DecimalLiteral(3.14), NodeInfo.totalDigits, 3)
    }
    it("Should calculate total digits of 3.14 as datatype literal") {
      checkOK(DatatypeLiteral("3.14", `xsd:decimal`), NodeInfo.totalDigits, 3)
    }
    it("Should calculate total digits of 3.123456 as datatype literal") {
      checkOK(DatatypeLiteral("3.123456", `xsd:decimal`), NodeInfo.totalDigits, 7)
    }
    it("Should calculate total digits of true and return error") {
      checkFails(BooleanLiteral(true), NodeInfo.totalDigits)
    }
  }
  describe("fractionDigits") {
    it(s"Fraction digits of 3.14"){
    checkOK(DecimalLiteral(3.14), NodeInfo.fractionDigits, 2)
    }
    it("Should calculate fraction digits of 3.14 as datatype literal") {
      checkOK(DatatypeLiteral("3.14", `xsd:decimal`), NodeInfo.fractionDigits, 2)
    }
    it("Should calculate fraction digits of 3.123456 as datatype literal") {
      checkOK(DatatypeLiteral("3.123456", `xsd:decimal`), NodeInfo.fractionDigits, 6)
    }
    it("Should calculate fraction digits of true and return 0") {
      checkFails(BooleanLiteral(true), NodeInfo.fractionDigits)
    }
  }

  def checkOK[A,B](x: A, f: (A, RDFReader) => IO[B], expected: B): Unit = {
    val ioa = RDFAsJenaModel.empty.flatMap(_.use(rdf => f(x,rdf)))
    ioa.attempt.unsafeRunSync.fold(
      e => fail("Failed"),
      v => v should be(expected)
    )
  }

  def checkFails[A,B](x: A, f: (A, RDFReader) => IO[B]): Unit = {
    val iob = RDFAsJenaModel.empty.flatMap(_.use(rdf => f(x,rdf)))
    iob.attempt.unsafeRunSync.fold(
      e => info("Failed as expected"),
      v => fail(s"Should have failed")
    )
  }

}
