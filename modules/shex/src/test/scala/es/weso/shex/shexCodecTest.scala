package es.weso.shex

import munit._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._
import es.weso.shex.implicits.eqShEx._
import es.weso.rdf.operations.Comparisons._
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import cats._
import cats.implicits._
import es.weso.rdf.nodes._

class shexCodecTest extends FunSuite {

    test("Should parse prefix") {
      val str = "pepe"
      parsePrefix(str) match {
        case Right(p) => ()
        case Left(e) => fail(s"Error parsing $str: $e")
      }
    }
    test("Should parse lang string") {
      val str = "\"pepe\"@es"
      parseLang(str) match {
        case Right(p) => ()
        case Left(e) => fail(s"Error parsing $str: $e")
      }
    }


  codecValueTest[IRI](IRI("x"))
  codecValueTest[ShapeLabel](IRILabel(IRI("http://example.org/")))
  codecValueTest[ShapeLabel](BNodeLabel(BNode("x")))
  

  codecValueTest[Max](IntMax(5))
  codecValueTest[Max](Star)
  codecStrTest[Max](""""-1"""", "-1")
  codecStrTest[Max](""""5"""", "5")
  codecStrTest[Max]("""5""", "5")

  codecValueTest[SemAct](SemAct(IRI("x"), None))
  codecValueTest[SemAct](SemAct(IRI("x"), Some("blah")))

  codecValueTest[ObjectLiteral](StringValue("hi"))

  codecValueTest[ShapeExpr](NodeConstraint(Some(IRILabel(IRI("http://example.org/a"))), Some(IRIKind), None, List(), None,None,None))
  codecValueTest[ShapeExpr](NodeConstraint(Some(IRILabel(IRI("http://example.org/a"))), Some(LiteralKind), None, List(), None,None,None))
  codecValueTest[ShapeExpr](NodeConstraint(Some(IRILabel(IRI("http://example.org/a"))), Some(NonLiteralKind), None, List(), None,None,None))
  codecValueTest[ShapeExpr](NodeConstraint(Some(IRILabel(IRI("http://example.org/a"))), Some(BNodeKind), None, List(), None,None,None))
  codecValueTest[ShapeExpr](NodeConstraint(Some(IRILabel(IRI("http://example.org/a"))), None, Some(IRI("http://datatype.org/int")), List(), None,None,None))
  codecValueTest[ShapeExpr](NodeConstraint(Some(IRILabel(IRI("http://example.org/a"))), None, Some(IRI("http://datatype.org/int")), List(Length(0)), None,None,None))
  codecValueTest[ShapeExpr](NodeConstraint(Some(IRILabel(IRI("http://example.org/a"))), None,
      Some(IRI("http://datatype.org/int")), List(Length(0), MinInclusive(NumericDouble(2.3,"2.3"))), None,None,None)
  )
  codecValueTest[ShapeExpr](
    NodeConstraint(
      Some(IRILabel(IRI("http://example.org/a"))),
      Some(BNodeKind), Some(IRI("http://datatype.org/int")),
      List(MinLength(2), MaxLength(5), Pattern("*.ex", None)),
      Some(List(StringValue("x"))),None,None))
  codecValueTest[ShapeExpr](ShapeRef(IRILabel(IRI("x")),None,None))
  codecValueTest[ShapeExpr](ShapeExternal(Some(IRILabel(IRI("http://example.org/a"))),None,None))
  codecValueTest[ShapeExpr](NodeConstraint(Some(IRILabel(IRI("http://example.org/a"))), None, None, List(), Some(List(DatatypeString("x", IRI("http://schema.org/boolean")))),None,None))

  def codecValueTest[A: Encoder: Decoder: Show: Eq](v: A)(implicit loc: munit.Location): Unit = {
    test(s"Should encode and decode ${v.show}") {
      val str = v.asJson.spaces4
      val result = decode[A](str)
      if (result === Right(v))
        ()
      else
        fail(s"Encoded value $v as $str was not equal to ${Right(v)}. Result: ${result}")
    }
  }

  def codecStrTest[A: Encoder: Decoder: Manifest: Eq](str: String, expected: String)(implicit loc: munit.Location): Unit = {
    test(s"Should decode $str and obtain $expected through decoder") { // of type ${manifest[A].runtimeClass.getSimpleName}") {
      decode[A](str).fold(
        e => fail(s"Error parsing $str: $e"),
        v => assertEquals(v.asJson.noSpaces, expected))
    }
  }

}
