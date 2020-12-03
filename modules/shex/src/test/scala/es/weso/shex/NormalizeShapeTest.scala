package es.weso.shex

import es.weso.rdf.nodes._
import es.weso.shex.normalized.{Constraint, NormalizedShape}
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.data._ 
import cats.implicits._
import cats.effect._
import NormalizedShape._

class NormalizeShapeTest extends AnyFunSpec with Matchers with EitherValues {

  describe(s"Normalize shape with IRI") {
    val shexStr =
      """
          |prefix : <http://example.org/>
          |:S { :p IRI }
          |""".stripMargin

    val ex      = IRI("http://example.org/")
    val p: Path = Direct(ex + "p")
    val a       = ex + "S"
    val iri     = NodeConstraint.nodeKind(IRIKind, List())
    val tc      = TripleConstraint.valueExpr(ex + "p", NodeConstraint.iri)
    shouldNormalizeShape(
      shexStr,
      a,
      NormalizedShape(Map(p -> Vector(Constraint(Some(iri), false, Cardinality(1, IntMax(1)), None, tc))), false)
    )
  }

  describe(s"Normalize shape with OR") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S { :p IRI Or BNode }
        |""".stripMargin

    val ex = IRI("http://example.org/")
    // TODO. Check if we could normalize these kind of shapes
    val p: Path              = Direct(ex + "p")
    val a                    = ex + "S"
    val iri                  = NodeConstraint.nodeKind(IRIKind, List())
    val bNode                = NodeConstraint.nodeKind(BNodeKind, List())
    val or                   = ShapeOr(None, List(iri, bNode), None, None)
    val tc: TripleConstraint = TripleConstraint.valueExpr(ex + "p", or)

    shouldNormalizeShape(
      shexStr,
      a,
      NormalizedShape(Map(p -> Vector(Constraint(Some(or), false, Cardinality(1, IntMax(1)), None, tc))), false)
    )
  }

  describe(s"Normalize shape with EXTRA") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S EXTRA :p { :p IRI }
        |""".stripMargin

    val ex                   = IRI("http://example.org/")
    val p: Path              = Direct(ex + "p")
    val a                    = ex + "S"
    val iri                  = NodeConstraint.nodeKind(IRIKind, List())
    val tc: TripleConstraint = TripleConstraint.valueExpr(ex + "p", iri)

    shouldNormalizeShape(
      shexStr,
      a,
      NormalizedShape(Map(p -> Vector(Constraint(Some(iri), true, Cardinality(1, IntMax(1)), None, tc))), false)
    )
  }

  describe(s"Normalize shape with EXTRA value set ") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S EXTRA :p { 
        |  :p IRI ;
        |  :q .
        |}
        |""".stripMargin

    val ex                   = IRI("http://example.org/")
    val p: Path              = Direct(ex + "p")
    val q: Path              = Direct(ex + "q")
    val a                    = ex + "S"
    val iri                  = NodeConstraint.nodeKind(IRIKind, List())
    val tc1: TripleConstraint = TripleConstraint.valueExpr(ex + "p", iri)
    val tc2: TripleConstraint = TripleConstraint.emptyPred(ex+"q")

    shouldNormalizeShape(
      shexStr,
      a,
      NormalizedShape(Map(
        p -> Vector(Constraint(Some(iri), true, Cardinality(1, IntMax(1)), None, tc1)),
        q -> Vector(Constraint(None, false, Cardinality(1, IntMax(1)), None, tc2)),
        ), false)
    )
  }

  describe(s"Normalize shape simple") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S { :p IRI }
        |""".stripMargin

    val ex      = IRI("http://example.org/")
    val p: Path = Direct(ex + "p")
    val a       = ex + "S"
    val iri     = NodeConstraint.nodeKind(IRIKind, List())
    val tc      = TripleConstraint.valueExpr(ex + "p", iri)
    shouldNormalizeShape(
      shexStr,
      a,
      NormalizedShape(
        Map(p -> Vector(normalized.Constraint(Some(iri), false, Cardinality(1, IntMax(1)), None, tc))),
        false
      )
    )
  }

  describe(s"Normalize shape with annotation ") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S {
        | :p IRI // :a 1
        |}
        |""".stripMargin

    val ex      = IRI("http://example.org/")
    val p: Path = Direct(ex + "p")
    val a       = ex + "S"
    val iri     = NodeConstraint.nodeKind(IRIKind, List())
    val xsd     = IRI("http://www.w3.org/2001/XMLSchema#")
    val as      = Some(List(Annotation(ex + "a", DatatypeString("1", xsd + "integer"))))
    val tc      = TripleConstraint.valueExpr(ex + "p", iri).copy(annotations = as)
    shouldNormalizeShape(
      shexStr,
      a,
      NormalizedShape(
        Map(p -> Vector(normalized.Constraint(Some(iri), false, Cardinality(1, IntMax(1)), as, tc))),
        false
      )
    )
  }

  describe(s"Normalize shape with top level annotation ") {
    pending
    /*
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S IRI {
        | :p IRI // :a 2
        |}
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val p: Path = Direct(ex + "p")
    val a = ex + "S"
    val iri = NodeConstraint.nodeKind(IRIKind,List())
    val xsd = IRI("http://www.w3.org/2001/XMLSchema#")
    val as = Some(List(Annotation(ex + "a",DatatypeString("1",xsd + "integer"))))
    val tc = TripleConstraint.valueExpr(ex + "p",iri).copy(annotations = as)

    // TODO: Add a test to check normalized shape
   */
  }

  def shouldNormalizeShape(strSchema: String, shapeLabel: IRI, expected: NormalizedShape): Unit = {
    it(s"Should normalize $shapeLabel and return ${expected.show}") {
      val shapeLbl = IRILabel(shapeLabel)
      val result = for {
        schema <- EitherT.liftF(Schema.fromString(strSchema))
        shape  <- EitherT.fromEither[IO](schema.getShape(shapeLbl))
        normalized <- EitherT.fromEither[IO](shape match {
          case s: Shape => s.normalized(Schema.empty)
          case _        => Left(s"$shape is not a plain shape")
        })
      } yield normalized
      result.value.unsafeRunSync.fold(e => fail(s"Error: $e"), n => 
        if (n == expected) info(s"Normalized shapes are equal")
        else {
         pprint.log(n,"obtained")
         pprint.log(expected,"expected")
         fail(s"""|Normalize shape different from expected
                      |Result:
                      |${n.show}
                      |Expected
                      |${expected.show}
                      |""".stripMargin)
        }
      )
    }
  }

  def shouldNotNormalizeShape(strSchema: String, shapeLabel: IRI): Unit = {
    it(s"Should not normalize $shapeLabel") {
      val shapeLbl = IRILabel(shapeLabel)
      val result = for {
        schema <- EitherT.liftF(Schema.fromString(strSchema))
        shape  <- EitherT.fromEither[IO](schema.getShape(shapeLbl))
        normalized <- EitherT.fromEither[IO](shape match {
          case s: Shape => s.normalized(Schema.empty)
          case _        => Left(s"$shape is not a plain shape")
        })
      } yield normalized
      result.value.unsafeRunSync.fold(
        e => info(s"Could not normalize shape with error $e as expected"),
        n => fail(s"It was able to normalize shape and return $n but it should have failed")
      )
    }
  }
}
