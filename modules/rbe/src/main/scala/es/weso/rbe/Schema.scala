package es.weso.rbe
import es.weso.graph._

/** Defines a Schema which is a map from Labels to Shapes
  */
case class Schema[Edge, Node, Label, Err, Evidence](
    m: Map[Label, Shape[DirectedEdge[Edge], Node, Label, Err, Evidence]],
    ignored: Seq[DirectedEdge[Edge]] // Edges that are ignored in closed schemas
) {

  type Shape_ = Shape[DirectedEdge[Edge], Node, Label, Err, Evidence]

  def lookup(label: Label): Option[Shape_] =
    m.get(label)

}

object Schema {
  def empty[Edge, Node, Label, Err, Evidence](): Schema[Edge, Node, Label, Err, Evidence] =
    Schema(Map(), Seq())

}
