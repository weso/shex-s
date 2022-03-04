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
      .addShape(Shape.empty.copy(id = Some(IRILabel(IRI("S")))))
      .withLabelLocationMap(Some(Map(IRILabel(IRI("S")) -> Location(line = 1, col = 0, tokenType = "label"))))
  )

  shouldParse(
    s"<S> extends @<T> { }",
    None,
    Schema.empty
      .addShape(
        Shape.empty.copy(id = Some(IRILabel(IRI("S"))), closed = Some(false), _extends = Some(List(IRILabel(IRI("T")))))
      )
      .withLabelLocationMap(Some(Map(IRILabel(IRI("S")) -> Location(line = 1, col = 0, tokenType = "label"))))
  )

  def shouldParse(str: String, base: Option[String], expected: Schema): Unit = {
    test(str) {
      Parser.parseSchema(str, base.map(IRI(_))) match {
        case Left(e) => {
          pprint.log(e, "Error parsing")
          fail(s"Failed to parse with error: $e")
        }
        case Right(parsedSchema) => {
          pprint.log(parsedSchema, "parsedSchema")
          assertEquals(parsedSchema, expected)
        }
      }
    }
  }

}
