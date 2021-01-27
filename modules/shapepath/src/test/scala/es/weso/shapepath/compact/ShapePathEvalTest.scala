package es.weso.shapepath.compact
import es.weso.rdf.nodes.IRI
import es.weso.shex.{IRILabel, Schema}
import munit.FunSuite
import es.weso.shapepath._
import es.weso.rdf.PrefixMap
import es.weso.rdf.Prefix
import cats.effect.IO
import cats._ 
import cats.implicits._

class ShapePathEvalTest extends FunSuite {
  shouldEval(s"@<S>/<p>",
    s"""|<S> { <p> . }""".stripMargin,
    Value(List())
  )
//    shouldParse(s"1", ShapePath(false,List(ExprStep(None,IntTripleExprIndex(1)))))
//    shouldParse(s"1/2", ShapePath(false,List(ExprStep(None,IntTripleExprIndex(1)),ExprStep(None,IntTripleExprIndex(2)))))
//    shouldParse(s"1/@2", ShapePath(false,List(ExprStep(None,IntTripleExprIndex(1)),ExprStep(None,IntShapeIndex(2)))))
//    shouldParse(s"/1/@2", ShapePath(true,List(ExprStep(None,IntTripleExprIndex(1)),ExprStep(None,IntShapeIndex(2)))))
//    shouldParse("/shape", ShapePath(true,List(ContextStep(ShapeCtx))))

  def shouldEval(
    str: String,
    schemaStr: String,
    expected: Value
    ): Unit = {

  test (s"Should parse $str with schema $schemaStr and obtain ${expected.toString}") {
    val cmp = for {
      schema <- Schema.fromString(schemaStr)
      shapePath <- IO.fromEither(ShapePath.fromString(str,"compact", None, schema.prefixMap).leftMap(
        err => new RuntimeException(s"Error parsing shapePath: ${err}\nstr: ${str}")))
      value <- ShapePath.eval(shapePath,schema,None) match {
        case (Nil, value) => IO.pure(value)
        case (ls, value) => IO.raiseError(new RuntimeException(s"Errors produced: ${ls.map(e => e.toString).mkString("\n")}\nFinal value: ${value}"))
      }
    } yield value
    cmp.attempt.unsafeRunSync().fold(
      err => fail(s"Error computing value: ${err.getMessage}"), 
      value => assertEquals(value, expected)
    )
  }
  }
}