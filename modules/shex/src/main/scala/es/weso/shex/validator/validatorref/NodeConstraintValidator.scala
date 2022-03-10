package es.weso.shex.validator.validatorref

import es.weso.shex.NodeConstraint
import cats.effect.IO
import cats.data.Validated
import es.weso.rdf.nodes.RDFNode

object NodeConstraintValidator {
    def validateNodeConstraint(node: RDFNode, nc: NodeConstraint): Validated[NodeConstraintError, RDFNode] = ???
}
