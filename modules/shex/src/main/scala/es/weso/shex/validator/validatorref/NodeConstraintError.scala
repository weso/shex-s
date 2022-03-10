package es.weso.shex.validator.validatorref

package es.weso.shex.validator.validatorref

import es.weso.shex.NodeConstraint
import cats.effect.IO
import es.weso.rdf.nodes.RDFNode
import cats.data.Validated

object NodeConstraintValidator {
    def validateNodeConstraint(node: RDFNode, nc: NodeConstraint): Validated[NodeConstraintError, RDFNode] = ???
}