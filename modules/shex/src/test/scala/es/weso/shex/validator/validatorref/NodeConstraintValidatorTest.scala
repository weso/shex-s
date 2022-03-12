package es.weso.shex.validator.validatorref

import munit._
import cats._
import es.weso.shex._
import es.weso.rdf.nodes._

class NodeConstraintValidatorTest extends FunSuite {

  
 test("IRIKind ok") {
  val nc = NodeConstraint.nodeKind(IRIKind, List())
  val node = IRI(s"http://example.org")
  assertEquals(NodeConstraintValidator.validateNodeConstraint(node,nc).isValid, true)
 }

 test("IRIKind not ok") {
  val nc = NodeConstraint.nodeKind(IRIKind, List())
  val node = BNode("1")
  assertEquals(NodeConstraintValidator.validateNodeConstraint(node,nc).isValid, false)
 }

 test("Datatype Integer ok") {
  val nc = NodeConstraint.datatype(IRI("http://www.w3.org/2001/XMLSchema#integer"), List())
  val node = IntegerLiteral(3)
  assertEquals(NodeConstraintValidator.validateNodeConstraint(node,nc).isValid, true)
 }

 test("Datatype Integer not ok") {
  val nc = NodeConstraint.datatype(IRI("http://www.w3.org/2001/XMLSchema#integer"), List())
  val node = StringLiteral("Hi")
  assertEquals(NodeConstraintValidator.validateNodeConstraint(node,nc).isValid, false)
 }
}
