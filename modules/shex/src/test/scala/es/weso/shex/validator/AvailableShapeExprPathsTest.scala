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

/*  {
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
    val sA = Shape.empty.withExpr(p(1))
    val A = ShapeDecl(_A, sA)
    
    val _B = IRILabel(ex + "B")
    val sB = Shape.empty.withExtends(_A).withExpr(p(2))
    val B = ShapeDecl(_B, sB)
    
    val _C = IRILabel(ex + "C")
    val sC = Shape.empty.withExtends(_A).withExpr(p(3))
    val C = ShapeDecl(_C, sC)
    
    val _D = IRILabel(ex + "D")
    val sD = Shape.empty.withExtends(_B, _C).withExpr(p(4))
    val D = ShapeDecl(_D, sD)
    
    val schema = Schema.emptyWithId(ex + "Schema").withShapes(A, B, C, D)
    
    val expected: List[(ShapeExpr, Available[Path])] = List(
      (sD, Available(Set(Direct(_p)))),
      (sB, Available(Set(Direct(_p)))),
      (sA, Available(Set(Direct(_p)))),
      (sC, Available(Set(Direct(_p))))
    )
    shouldMatchAvailableShapeExprPaths(msg, schema, sD, Some(_D), expected)
  }
*/
  {
    val msg = """Reclined Example""".stripMargin
    val ex = IRI("http://e/")
    val _code = ex + "code"
    def code(str: String): TripleExpr =
      TripleConstraint.emptyPred(_code).withValueExpr(NodeConstraint.empty.withValues(stringValue(str)))
    
    val _Obs = IRILabel(ex + "Obs")
    val sObs = Shape.empty
    val Obs = ShapeDecl(_Obs, sObs)
    
    val _Vital = IRILabel(ex + "Vital")
    val sVital = Shape.empty.withExtends(_Obs)
    val Vital = ShapeDecl(_Vital, sVital)
    
    val _Posture = IRILabel(ex + "Posture")
    val sPosture = Shape.empty.withExpr(code("posture"))
    val Posture = ShapeDecl(_Posture, sPosture)

    val _Reclined = IRILabel(ex + "Reclined")
    val sReclined = Shape.empty.withExtends(_Posture)
    val Reclined = ShapeDecl(_Reclined, sReclined)
    
    val _PostureVital = IRILabel(ex + "PostureVital")
    val sPostureVital = Shape.empty.withExtends(_Posture, _Vital)
    val PostureVital = ShapeDecl(_PostureVital, sPostureVital)
    
    val _ReclinedVital = IRILabel(ex + "ReclinedVital")
    val sReclinedVital = Shape.empty.withExtends(_PostureVital, _Reclined)
    val ReclinedVital = ShapeDecl(_ReclinedVital, sReclinedVital)

    val _BP = IRILabel(ex + "BP")
    val sBP = ShapeExpr.and(Shape.empty.withExtends(_Vital), Shape.empty.withExpr(code("bp")))
    val BP = ShapeDecl(_BP, sBP)

    val _ReclinedBP = IRILabel(ex + "ReclinedBP")
    val sReclinedBP = Shape.empty.withExtends(_BP, _ReclinedVital)
    val ReclinedBP = ShapeDecl(_ReclinedBP, sReclinedBP)

    val schema = Schema
     .emptyWithId(ex + "Schema")
     .withShapes(Obs, Vital, Posture, Reclined, PostureVital,  ReclinedVital,  BP, ReclinedBP)
    
    val expectedObs: List[(ShapeExpr, Available[Path])] = List(
      (sObs, Available(Set()))
    )
    val expectedVital: List[(ShapeExpr, Available[Path])] = List(
      (sVital, Available(Set())),
      (sObs, Available(Set()))
    )
    val expectedPosture: List[(ShapeExpr, Available[Path])] = List(
      (sPosture, Available(Set(Direct(_code)))),
    )
    val expectedReclined: List[(ShapeExpr, Available[Path])] = List(
      (sReclined, Available(Set())),
      (sPosture, Available(Set(Direct(_code))))
    )
    val expectedReclinedBP: List[(ShapeExpr, Available[Path])] = List(
      (sReclinedBP, Available(Set())),
      (sBP, Available(Set(Direct(_code)))),
      (sReclinedVital, Available(Set()))
    )
    
//    shouldMatchAvailableShapeExprPaths(msg, schema, sObs, Some(_Obs), expectedObs)
//1    shouldMatchAvailableShapeExprPaths(msg, schema, sVital, Some(_Vital), expectedVital)
//2    shouldMatchAvailableShapeExprPaths(msg, schema, sPosture, Some(_Posture), expectedPosture)
//3    shouldMatchAvailableShapeExprPaths(msg, schema, sReclined, Some(_Reclined), expectedReclined)
    shouldMatchAvailableShapeExprPaths(msg, schema, sReclinedBP, Some(_ReclinedBP), expectedReclinedBP)
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
      s"""|AvailableShapeExprPathsTest: ${msg}
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
