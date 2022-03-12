package es.weso.shex.validator.validatorref

import es.weso.shapemaps.FixedShapeMap
import es.weso.rdf.nodes.RDFNode
import es.weso.shapemaps.ShapeMapLabel
import es.weso.shapemaps.Info
import es.weso.rdf.PrefixMap
import es.weso.utils.internal.CollectionCompat._
import es.weso.shapemaps.Status._

case class State(
    shapeMap: Map[RDFNode, Map[ShapeMapLabel, Info]],
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap,
    pending: Set[(RDFNode, ShapeMapLabel, Info)]
) {

  def changePending(node: RDFNode, shapeLabel: ShapeMapLabel, info: Info): State =
    this.copy(pending = pending - ((node, shapeLabel, info)))
}

object State {

  private def findPending(fm: Map[RDFNode, Map[ShapeMapLabel, Info]]): Set[(RDFNode, ShapeMapLabel, Info)] = {
    fm.toList
      .map { case (n, m) => m.toList.map { case (lbl, i) => (n, lbl, i) } }
      .flatten
      .filter { case (n, lbl, i) => i.status == PendingConforms || i.status == PendingDoesntConform }
      .toSet
  }

  def fromFixedMap(fm: FixedShapeMap): State = {
    val newMap = mapValues(fm.shapeMap)(mv => mapValues(mv)(changeConformByPending))
    State(
      shapeMap = newMap,
      nodesPrefixMap = fm.nodesPrefixMap,
      shapesPrefixMap = fm.shapesPrefixMap,
      pending = findPending(newMap)
    )
  }

  private def changeConformByPending(i: Info): Info = i.copy(status = i.status match {
    case Conformant    => PendingConforms
    case NonConformant => PendingDoesntConform
    case other         => other
  })

}
