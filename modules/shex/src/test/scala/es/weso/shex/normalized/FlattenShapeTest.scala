package es.weso.shex.normalized

import es.weso.rdf.nodes._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.data._ 
import cats.implicits._
import cats.effect._
import es.weso.shex._
import FlatShape._

class FlattenShapeTest extends AnyFunSpec with Matchers with EitherValues {

  describe(s"Flatten shape with IRI") {
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
    shouldFlattenShape(
      shexStr,
      a,
      Some(FlatShape(Map(p -> Constraint(Some(iri), false, Cardinality(1, IntMax(1)), None, tc)), false))
    )
  }

/*  describe(s"Normalize shape with OR") {
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

    shouldFlattenShape(
      shexStr,
      a,
      Some(FlatShape(Map(p -> Constraint(Some(or), false, Cardinality(1, IntMax(1)), None, tc)), false))
    )
  }
*/
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

    shouldFlattenShape(
      shexStr,
      a,
      Some(FlatShape(Map(p -> Constraint(Some(iri), true, Cardinality(1, IntMax(1)), None, tc)), false))
    )
  }

  describe(s"Flatten shape with EXTRA value set ") {
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
    val any                  = ShapeExpr.any
    val tc1: TripleConstraint = TripleConstraint.valueExpr(ex + "p", iri)
    val tc2: TripleConstraint = TripleConstraint.valueExpr(ex + "q", any)

    shouldFlattenShape(
      shexStr,
      a,
      Some(FlatShape(Map(
        p -> Constraint(Some(iri), true, Cardinality(1, IntMax(1)), None, tc1),
        q -> Constraint(Some(any), false, Cardinality(1, IntMax(1)), None, tc2)
        ), false))
    )
  }

  describe(s"Flatten shape simple") {
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
    shouldFlattenShape(
      shexStr,
      a,
      Some(FlatShape(Map(p -> Constraint(Some(iri), false, Cardinality(1, IntMax(1)), None, tc)),false))
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
    shouldFlattenShape(
      shexStr,
      a,
      Some(FlatShape(
        Map(p -> Constraint(Some(iri), false, Cardinality(1, IntMax(1)), as, tc)),
        false
      ))
    )
  }

  describe(s"Flattn shape with Ref false") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S EXTRA :p { :p @:T }
        |:T {}
        |""".stripMargin

    val ex                   = IRI("http://example.org/")
    // val p: Path              = Direct(ex + "p")
    val s: IRI               = ex + "S"
    // val iri                  = NodeConstraint.nodeKind(IRIKind, List())
    // val tc: TripleConstraint = TripleConstraint.valueExpr(ex + "p", ShapeRef(s))

    shouldFlattenShape(
      shexStr,
      s,
      None //      Some(FlatShape(Map(p -> Constraint(Some(iri), true, Cardinality(1, IntMax(1)), None, tc)), false))
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

  def shouldFlattenShape(strSchema: String, shapeLabel: IRI, maybeExpected: Option[FlatShape]): Unit = {
    it(s"Should flatten $shapeLabel and return ${maybeExpected.show}") {
      val shapeLbl = IRILabel(shapeLabel)
      val result: EitherT[IO,String,FlatShape] = for {
        schema <- EitherT.liftF(Schema.fromString(strSchema))
        shapeExpr  <- EitherT.fromEither[IO](schema.getShape(shapeLbl))
        shape <- EitherT.fromEither[IO](shapeExpr match {
          case s: Shape => s.asRight
          case other => s"Should be a Shape and it is: $other".asLeft
        })
        flattenShape <- EitherT.fromEither[IO](shape match {
          case s: Shape => FlatShape.fromShape(shape,schema)
          case _        => Left(s"$shape is not a plain shape")
        })
      } yield flattenShape
      val eitherFlattenShape = result.value.unsafeRunSync
      (eitherFlattenShape,maybeExpected) match {
        case (Left(s), None) => info(s"Failed to flatten shape as expected. Error: $s")
        case (Right(s), Some(expected)) => if (s == expected) 
         info(s"Flatten shapes are equal")
         else 
         fail(s"FlatShape different from expected\nResult:\n${s.show}\nExpected\n${expected.show}\nResult\n${s}\nExpected:\n${expected}")
        case (Left(s), Some(expected)) => fail(s"Should flatten but returned error: $s\n Expected was:\n${expected.show}") 
        case (Right(s), None) => fail(s"Should not flatten but returned flatShape:\n${s.show}")
      }
    }
  }

}
