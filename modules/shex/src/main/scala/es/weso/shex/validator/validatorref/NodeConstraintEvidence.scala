package es.weso.shex.validator.validatorref

import es.weso.shex._
import es.weso.rdf.nodes._

sealed abstract class NodeConstraintEvidence protected (val msg: String)
    extends Product
    with Serializable
case class IRIKindEvidence(node: RDFNode) extends NodeConstraintEvidence(s"${node} is an IRI")
case class BNodeKindEvidence(node: RDFNode)
    extends NodeConstraintEvidence(s"${node} is a BlankNode")
case class LiteralKindEvidence(node: RDFNode)
    extends NodeConstraintEvidence(s"${node} is a Literal")
case class NonLiteralKindEvidence(node: RDFNode)
    extends NodeConstraintEvidence(s"${node} is a NonLiteral")

case class DatatypeEvidence(node: RDFNode, datatype: IRI)
    extends NodeConstraintEvidence(s"${node} has datatype ${datatype}")
case class ValueSet(node: RDFNode, conforms: List[ValueSetValue], valueSet: List[ValueSetValue])
    extends NodeConstraintEvidence(
      s"${node} conforms to values [${conforms.map(_.toString).mkString(",")}] for valueSet: ${valueSet.map(_.toString).mkString(",")}"
    )
