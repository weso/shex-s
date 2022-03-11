package es.weso.shex.validator.validatorref

import scala.util.control.NoStackTrace
import es.weso.rdf.nodes._

sealed abstract class NodeConstraintError protected (val msg: String) extends Exception(msg) with NoStackTrace with Product with Serializable 

case class IRIKindError(node: RDFNode) extends NodeConstraintError(s"IRIKindError: ${node} is not an IRI")
case class BNodeKindError(node: RDFNode) extends NodeConstraintError(s"BNodeKindError: ${node} is not a BlankNode")
case class LiteralKindError(node: RDFNode) extends NodeConstraintError(s"LiteralKindError: ${node} is not a Literal")
case class NonLiteralKindError(node: RDFNode) extends NodeConstraintError(s"NonLiteralKindError: ${node} is not a NonLiteral")

case class LiteralNonDatatypeError(node: RDFNode, datatype: IRI, expectedDatatype: IRI) extends NodeConstraintError(s"LiteralNonDatatypeError: $node has datatype $datatype instead of expected $expectedDatatype")
case class DatatypeErrorNonLiteral(node: RDFNode, expectedDatatype: IRI) extends NodeConstraintError(s"LiteralNonDatatypeError: $node is not literal, and doesn't datatype $expectedDatatype")

case class NotImplementedNodeConstraintError(node: RDFNode, message: String) extends NodeConstraintError(s"NotImplementedNodeConstraint on $node: $message")