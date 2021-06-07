package es.weso.shapepath.compact
import es.weso.rdf.nodes.IRI
import es.weso.shex.{IRILabel, Schema}
import munit.FunSuite
import es.weso.shapepath._
import es.weso.rdf.PrefixMap
import es.weso.rdf.Prefix

class ShapePathCompactTest extends FunSuite {
  shouldParse(s"/<p>", ShapePath(true,List(Step.fromIRI(IRI("p")))))
  shouldParse(s"<p>", ShapePath(false,List(Step.fromIRI(IRI("p")))))
  shouldParse(s"@<S>", ShapePath(true,List(
    Step.mkStep(Some(NestedShapeExpr), EqName(IRI("S"))))))
  shouldParse(s"@<S>/<p>", 
    ShapePath(true,List(
      Step.mkStep(Some(NestedShapeExpr), EqName(IRI("S"))),
      Step.fromIRI(IRI("p"))
    ))) 
  shouldParse(s":p", 
    ShapePath(false,List(
      Step.fromIRI(IRI("http://example.org/p"))
    )), PrefixMap.empty.addPrefix(Prefix(""),IRI("http://example.org/")))
//    shouldParse(s"1", ShapePath(false,List(ExprStep(None,IntTripleExprIndex(1)))))
//    shouldParse(s"1/2", ShapePath(false,List(ExprStep(None,IntTripleExprIndex(1)),ExprStep(None,IntTripleExprIndex(2)))))
//    shouldParse(s"1/@2", ShapePath(false,List(ExprStep(None,IntTripleExprIndex(1)),ExprStep(None,IntShapeIndex(2)))))
//    shouldParse(s"/1/@2", ShapePath(true,List(ExprStep(None,IntTripleExprIndex(1)),ExprStep(None,IntShapeIndex(2)))))
//    shouldParse("/shape", ShapePath(true,List(ContextStep(ShapeCtx))))

  def shouldParse(
    str: String, 
    expected: ShapePath, 
    prefixMap: PrefixMap = PrefixMap.empty
    ): Unit = {

  test (s"Should parse $str and obtain ${expected.toString}") {
    ShapePath.fromString(str,"compact",None, prefixMap).fold(
      err => fail(s"Error parsing $str\n$err"),
      sp => assertEquals(sp, expected)
    )
  }
  }
}