package es.weso.shex.validator

import es.weso.rdf.nodes._
import cats._
import cats.implicits._
import es.weso.rdf.PrefixMap
import es.weso.shex.implicits.showShEx._

case class NodeShape(node: RDFNode, st: ShapeType) {
  override def toString = NodeShape.nodeShapeShow.show(this)

  def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String =
    s"${nodesPrefixMap.qualify(node)}@${st.label.map(label => shapesPrefixMap.qualify(label.toRDFNode)).getOrElse(st.se.show)}"
}

object NodeShape {

  implicit val nodeShapeShow: Show[NodeShape] = new Show[NodeShape] {
    def show(ns: NodeShape) =
      s"[${ns.node},${ns.st.label.map(label => label.toRDFNode.toString).getOrElse(ns.st.se.show)}]"
  }

}
