package es.weso.shapepath.compact
import es.weso.rdf.nodes.IRI
import es.weso.shex.{IRILabel, Schema}
import munit.FunSuite
import es.weso.shapepath._

class ShapePathCompactTest extends FunSuite {
  shouldParse(s"/<p>", ShapePath(true,List(Step.fromIRI(IRI("p")))))
//    shouldParse(s"/@<S>", ShapePath(true,List(Step.fromShapeLabel(IRI("S")))))
//    shouldParse(s"@<S>", ShapePath(false,List(Step.fromShapeLabel(IRI("S")))))
//    shouldParse(s"1", ShapePath(false,List(ExprStep(None,IntTripleExprIndex(1)))))
//    shouldParse(s"1/2", ShapePath(false,List(ExprStep(None,IntTripleExprIndex(1)),ExprStep(None,IntTripleExprIndex(2)))))
//    shouldParse(s"1/@2", ShapePath(false,List(ExprStep(None,IntTripleExprIndex(1)),ExprStep(None,IntShapeIndex(2)))))
//    shouldParse(s"/1/@2", ShapePath(true,List(ExprStep(None,IntTripleExprIndex(1)),ExprStep(None,IntShapeIndex(2)))))
//    shouldParse("/shape", ShapePath(true,List(ContextStep(ShapeCtx))))

  def shouldParse(str: String, expected: ShapePath): Unit = {
    test (s"Should parse $str and obtain ${expected.toString}") {
    ShapePath.fromString(str,"compact",None).fold(
      err => fail(s"Error parsing $str\n$err"),
      sp => assertEquals(sp, expected)
    )
  }
  }
}