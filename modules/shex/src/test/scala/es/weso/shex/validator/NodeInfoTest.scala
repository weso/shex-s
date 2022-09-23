package es.weso.shex.validator

import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.jena.RDFAsJenaModel
import munit._
import es.weso.rdf.RDFReader
import cats.effect.IO
// import cats.data._
// import es.weso.utils.IOUtils._

class NodeInfoTest extends CatsEffectSuite {
  // val rdf = RDFAsJenaModel.empty

  test("Should calculate total digits of 3.14") {
    checkOK(DecimalLiteral(3.14), NodeInfo.totalDigits, 3)
  }

  test("Should calculate total digits of 3.14 as datatype literal") {
      checkOK(DatatypeLiteral("3.14", `xsd:decimal`), NodeInfo.totalDigits, 3)
  }

  test("Should calculate total digits of 3.123456 as datatype literal") {
      checkOK(DatatypeLiteral("3.123456", `xsd:decimal`), NodeInfo.totalDigits, 7)
    }
  test("Should calculate total digits of true and return error") {
      checkFails(BooleanLiteral(true), NodeInfo.totalDigits)
    }
  test(s"Fraction digits of 3.14"){
    checkOK(DecimalLiteral(3.14), NodeInfo.fractionDigits, 2)
    }

  test("Should calculate fraction digits of 3.14 as datatype literal") {
      checkOK(DatatypeLiteral("3.14", `xsd:decimal`), NodeInfo.fractionDigits, 2)
    }
  
  test("Should calculate fraction digits of 3.123456 as datatype literal") {
      checkOK(DatatypeLiteral("3.123456", `xsd:decimal`), NodeInfo.fractionDigits, 6)
  }

  test("Should calculate fraction digits of 5.2E0 as datatype literal and fail") {
      checkFails(DatatypeLiteral("5.2E0", `xsd:double`), NodeInfo.fractionDigits)
  }

  test("Should calculate fraction digits of 5.2E0 and fail") {
      checkFails(DoubleLiteral(5.2E0, "5.2E0"), NodeInfo.fractionDigits)
  }


  test("Should calculate fraction digits of true and return 0") {
      checkFails(BooleanLiteral(true), NodeInfo.fractionDigits)
    }

  def checkOK[A,B](x: A, f: (A, RDFReader) => IO[B], expected: B): Unit = {
    import cats.effect.unsafe.implicits.global
    val ioa = RDFAsJenaModel.empty.flatMap(_.use(rdf => f(x,rdf)))
    ioa.attempt.unsafeRunSync().fold(
      e => fail(s"Failed with error $e"),
      v => assertEquals(v, expected)
    )
  }

  def checkFails[A,B](x: A, f: (A, RDFReader) => IO[B]): Unit = {
    import cats.effect.unsafe.implicits.global
    val iob = RDFAsJenaModel.empty.flatMap(_.use(rdf => f(x,rdf)))
    iob.attempt.unsafeRunSync().fold(
      e => (),
      v => fail(s"Should have failed but obtained: $v")
    )
  }

}