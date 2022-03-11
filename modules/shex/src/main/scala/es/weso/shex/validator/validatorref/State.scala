package es.weso.shex.validator.validatorref

import es.weso.shapemaps.FixedShapeMap
import es.weso.rdf.nodes.RDFNode
import es.weso.shapemaps.ShapeMapLabel
import es.weso.shapemaps.Info
import es.weso.rdf.PrefixMap

case class State(
    shapeMap: Map[RDFNode, Map[ShapeMapLabel, Info]],
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap,
    pending: Set[(RDFNode, ShapeMapLabel)]
) {

  def changePending(node: RDFNode, shapeLabel: ShapeMapLabel): State =
    this.copy(pending = pending - ((node, shapeLabel)))
}

object State {

  private def findPending(fm: FixedShapeMap): Set[(RDFNode, ShapeMapLabel)] = Set()

  def fromFixedMap(fm: FixedShapeMap): State = State(
    shapeMap = fm.shapeMap,
    nodesPrefixMap = fm.nodesPrefixMap,
    shapesPrefixMap = fm.shapesPrefixMap,
    pending = findPending(fm)
  )
}
