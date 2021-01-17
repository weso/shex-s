package es.weso.shapepath.compact
import es.weso.rdf.nodes.IRI
import es.weso.shex.{IRILabel, Schema}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._
import es.weso.shapepath._

class ShapePathCompactTest extends AnyFunSpec with Matchers {
  describe(s"Compact parser test") {
    shouldParse(s"/<p>", ShapePath(true,List(ExprStep(None,LabelTripleExprIndex(IRILabel(IRI("p")), None)))))
    shouldParse(s"/@<S>", ShapePath(true,List(ExprStep(None,ShapeLabelIndex(IRILabel(IRI("S")))))))
    shouldParse(s"@<S>", ShapePath(false,List(ExprStep(None,ShapeLabelIndex(IRILabel(IRI("S")))))))
    shouldParse(s"1", ShapePath(false,List(ExprStep(None,IntTripleExprIndex(1)))))
    shouldParse(s"1/2", ShapePath(false,List(ExprStep(None,IntTripleExprIndex(1)),ExprStep(None,IntTripleExprIndex(2)))))
    shouldParse(s"1/@2", ShapePath(false,List(ExprStep(None,IntTripleExprIndex(1)),ExprStep(None,IntShapeIndex(2)))))
    shouldParse(s"/1/@2", ShapePath(true,List(ExprStep(None,IntTripleExprIndex(1)),ExprStep(None,IntShapeIndex(2)))))
    shouldParse("/shape", ShapePath(true,List(ContextStep(ShapeCtx))))
  }

  def shouldParse(str: String, expected: ShapePath): Unit = {
    it (s"Should parse $str and obtain ${expected.toString}") {
    ShapePath.fromString(str,"compact",None).fold(
      err => fail(s"Error parsing $str\n$err"),
      sp => if (sp == expected) info(s"OK!")
      else fail(s"Shape paths don't match\nParsed: \n$sp\nExpected:\n$expected")
    )
  }
  }
}