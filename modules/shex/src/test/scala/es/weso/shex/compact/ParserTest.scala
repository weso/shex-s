package es.weso.shex.compact

import es.weso.utils.json.JsonTest
import es.weso.rdf.locations.Location
import es.weso.rdf.nodes.IRI
import es.weso.shex._
import munit.FunSuite

class ParserTest extends FunSuite {

  shouldParse(
    s"<S> {}",
    None,
    Schema.empty
      .addShape(
        ShapeDecl(
          lbl = IRILabel(IRI("S")), 
          shapeExpr = Shape.empty, 
          _abstract = false
          ))
      .withLabelLocationMap(
        Some(Map(IRILabel(IRI("S")) -> Location(line = 1, col = 0, tokenType = "label")))
      )
  )

  shouldParse(
    s"<S> extends @<T> { }",
    None,
    Schema.empty
      .addShape(
        ShapeDecl(
          lbl = IRILabel(IRI("S")),
          shapeExpr = Shape.empty.copy(
            closed = Some(false),
           _extends = Some(List(IRILabel(IRI("T"))))
           ),
          _abstract = false
        )
      )
      .withLabelLocationMap(
        Some(Map(IRILabel(IRI("S")) -> Location(line = 1, col = 0, tokenType = "label")))
      )
  )

  def shouldParse(str: String, base: Option[String], expected: Schema): Unit =
    test(str) {
      Parser.parseSchema(str, base.map(IRI(_))) match {
        case Left(e) =>
          pprint.log(e, "Error parsing")
          fail(s"Failed to parse with error: $e")
        case Right(parsedSchema) =>
          if (parsedSchema != expected) {
            pprint.log(s"Schemas are different\nParsed: $parsedSchema\nexpected: $expected")
          }
          assertEquals(parsedSchema, expected)
      }
    }

}
