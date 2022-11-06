package es.weso.shex.validator

import es.weso.rdf.nodes._
import es.weso.shex._
import es.weso.shex.implicits.showShEx._
import cats.data._
import cats.effect._
import cats.implicits._
import cats._
import munit._
import es.weso.shex.validator.PartitionUtils._

class AvailableShapeExprPathsTest extends CatsEffectSuite with AvailableShapeExprPaths {

  {
    val msg = """|prefix : <http://e/
                 |
                 |:Reclined extends @:Posture {}
                 |:Posture { :p . }
                 |""".stripMargin
    val ex = IRI("http://e/")
    val p = ex + "p"
    val te = TripleConstraint.emptyPred(p)
    val _Reclined = IRILabel(ex + "Reclined")
    val _Posture = IRILabel(ex + "Posture")
    val s = Shape.empty.withExtends(_Posture)
    val Reclined = ShapeDecl(_Reclined, s)
    val postureSE = Shape.empty.withExpr(te)
    val Posture = ShapeDecl(_Posture, postureSE)
    val schema = Schema.emptyWithId(ex + "Schema").withShapes(Reclined, Posture)
    val expected: List[(ShapeExpr, Available[Path])] = List(
      (s, Available(Set())),
      (postureSE, Available(Set(Direct(p))))
    )
    shouldMatchAvailableShapeExprPaths(msg, schema, s, Some(_Reclined), expected)
  }

  {
    val msg = """|prefix : <http://e/
                 |
                 |:Reclined extends @:Posture {}
                 |:Posture { :p . }
                 |:ReclinedVital EXTENDS @:Reclined {}
                 |
                 |""".stripMargin
    val ex = IRI("http://e/")
    val p = ex + "p"
    val te = TripleConstraint.emptyPred(p)
    val _Reclined = IRILabel(ex + "Reclined")
    val _Posture = IRILabel(ex + "Posture")
    val s = Shape.empty.withExtends(_Posture)
    val Reclined = ShapeDecl(_Reclined, s)
    val postureSE = Shape.empty.withExpr(te)
    val Posture = ShapeDecl(_Posture, postureSE)
    val schema = Schema.emptyWithId(ex + "Schema").withShapes(Reclined, Posture)
    val expected: List[(ShapeExpr, Available[Path])] = List(
      (s, Available(Set())),
      (postureSE, Available(Set(Direct(p))))
    )
    shouldMatchAvailableShapeExprPaths(msg, schema, s, Some(_Reclined), expected)
  }

  def shouldMatchAvailableShapeExprPaths(
      msg: String,
      schema: Schema,
      s: Shape,
      parent: Option[ShapeLabel],
      expected: List[(ShapeExpr, Available[Path])]
  )(implicit
      loc: munit.Location
  ): Unit =
    test(
      s"""|AvailableShapeExprPathsTest: 
          |${msg}
          |Shape: ${s}
          |Expected: ${expected.map(_.toString).mkString("\n")}
          |""".stripMargin
    ) {
      assertIO(
        ResolvedSchema
          .resolve(schema)
          .flatMap(schema =>
            IO.println(s"""|Shape: $s
                           |Schema:
                           |$schema
                           |""".stripMargin) *>
              getAvailableShapeExprsPaths(s, schema, parent)
          ),
        expected
      )
    }

}
