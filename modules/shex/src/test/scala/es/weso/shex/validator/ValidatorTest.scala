package es.weso.shex.validator

// import es.weso.rdf.RDFReader
import es.weso.rdf.jena._
import es.weso.rdf.nodes._
import es.weso.shex._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ValidatorTest extends AnyFunSpec with Matchers with EitherValues {

  describe("ShEx validator") {
    val shapeLabel = IRILabel(IRI("http://example.org/S"))
    val schema =
      Schema.empty.copy(shapes = Some(List(NodeConstraint.nodeKind(IRIKind, List()).addId(shapeLabel))))
    val rdfStr = """|prefix : <http://example.org/>
                    |:a :p :b .""".stripMargin

    it("Should not validate if label not found") {
      shouldValidate(IRI("http://example.org/a"), IRILabel(IRI("http://example.org/Unknown")), rdfStr, schema, false)
    }

    it("Should validate single node constraint") {
      shouldValidate(IRI("http://example.org/a"), shapeLabel, rdfStr, schema, true)
    }
  }

  def shouldValidate(node: RDFNode, label: ShapeLabel, rdfStr: String, schema: Schema, ok: Boolean): Unit = {
    val result = RDFAsJenaModel.fromChars(rdfStr, "TURTLE").use(rdf => for {
      resolved <- ResolvedSchema.resolve(schema,None)
      v = Validator(resolved)
      check: ShExChecker.Check[ShapeTyping] = v.checkNodeLabel(node, label)
      r <- ShExChecker.runCheck(check, rdf)
    } yield r)
    result.attempt.unsafeRunSync.fold(
      e => fail(s"Failed: $e"), 
      r => r.toEither.fold(
        e => fail(s"Error obtaining result: $e"),
        res => if (ok) {
          if (res.hasType(node,label)) info(s"$node has type $label as expected")
          else fail(s"$node doesn't have type $label")
        } else {
          if (res.hasType(node,label)) fail(s"$node has type $label but it was expected otherwise")
          else info(s"$node doesn't have type $label as expected")
        }
      )
    )
  }


}
