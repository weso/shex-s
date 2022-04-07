package es.weso.shex.validator.validatorref

import munit._
import cats._
import es.weso.shex._
import es.weso.rdf.nodes._

class NodeConstraintValidatorTest extends CatsEffectSuite {

  test("IRIKind ok") {
    val nc   = NodeConstraint.nodeKind(IRIKind, List())
    val node = IRI(s"http://example.org")
    NodeConstraintValidator
      .validateNodeConstraint(node, nc)
      .map(v => assertEquals(v.isValid, true))
  }

  test("IRIKind not ok") {
    val nc   = NodeConstraint.nodeKind(IRIKind, List())
    val node = BNode("1")
    NodeConstraintValidator
      .validateNodeConstraint(node, nc)
      .map(v => assertEquals(v.isValid, false))
  }

  test("Datatype Integer ok") {
    val nc   = NodeConstraint.datatype(IRI("http://www.w3.org/2001/XMLSchema#integer"), List())
    val node = IntegerLiteral(3)
    NodeConstraintValidator
      .validateNodeConstraint(node, nc)
      .map(v => assertEquals(v.isValid, true))
  }

  test("Datatype Integer not ok") {
    val nc   = NodeConstraint.datatype(IRI("http://www.w3.org/2001/XMLSchema#integer"), List())
    val node = StringLiteral("Hi")
    NodeConstraintValidator
      .validateNodeConstraint(node, nc)
      .map(v => assertEquals(v.isValid, false))
  }

  test("ValueSet 2 in [1 2 3] ") {
    val nc =
      NodeConstraint.valueSet(List(ObjectValue.intValue(1), ObjectValue.intValue(2), ObjectValue.intValue(3)), List())
    val node = IntegerLiteral(2)
    NodeConstraintValidator
      .validateNodeConstraint(node, nc)
      .map(v => assertEquals(v.isValid, true))
  }

  test("ValueSet 4 in [1 2 3] not ok") {
    val nc =
      NodeConstraint.valueSet(List(ObjectValue.intValue(1), ObjectValue.intValue(2), ObjectValue.intValue(3)), List())
    val node = IntegerLiteral(4)
    NodeConstraintValidator
      .validateNodeConstraint(node, nc)
      .map(v => assertEquals(v.isValid, false))
  }
}
