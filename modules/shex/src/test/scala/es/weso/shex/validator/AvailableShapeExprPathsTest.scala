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
import ObjectValue._

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

  /*  {
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
  } */

  {
    val msg = """|prefix : <http://e/
                 |
                 |:A { :p [ 1] }
                 |:B extends @:A { :p [ 2 ] }
                 |:C extends @:A { :p [ 3 ] }
                 |:D extends @:B extends @:C { :p [ 4 ] } 
                 |
                 |""".stripMargin
    val ex = IRI("http://e/")
    val _p = ex + "p"
    def p(n: Int): TripleExpr =
      TripleConstraint.emptyPred(_p).withValueExpr(NodeConstraint.empty.withValues(intValue(n)))
    val _A = IRILabel(ex + "A")
    val _B = IRILabel(ex + "B")
    val _C = IRILabel(ex + "C")
    val _D = IRILabel(ex + "D")
    val A = ShapeDecl(_A, Shape.empty.withExpr(p(1)))
    val B = ShapeDecl(_B, Shape.empty.withExtends(_A).withExpr(p(2)))
    val C = ShapeDecl(_C, Shape.empty.withExtends(_A).withExpr(p(3)))
    val sD = Shape.empty.withExtends(_B, _C).withExpr(p(4))
    val D = ShapeDecl(_D, sD)
    val schema = Schema.emptyWithId(ex + "Schema").withShapes(A, B, C, D)
    val expected: List[(ShapeExpr, Available[Path])] = List(
      (sD, Available(Set(Direct(_p)))),
      (B, Available(Set(Direct(_p)))),
      (A, Available(Set(Direct(_p)))),
      (C, Available(Set(Direct(_p))))
    )
    shouldMatchAvailableShapeExprPaths(msg, schema, sD, Some(_D), expected)
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
          |${expected.length} expected: 
          |${expected.map(_.toString).mkString("\n")}
          |----
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
              getAvailableShapeExprsPaths(s, schema, parent).flatMap(r =>
                if (r != expected)
                  IO.println(s"""|Different....
                                 |${r.length} obtained: 
                                 |${r.map(_.toString).mkString("\n")}
                                 |---
                                 |""".stripMargin) *>
                    IO.pure(r)
                else IO.pure(r)
              )
          ),
        expected
      )
    }

}
