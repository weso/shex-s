package es.weso.graph

/** Generic representation of graphs
  */
trait Graph[Edge, Node] {

  type Neighs_ = Seq[Neigh[Edge, Node]]

  /** List of nodes
    */
  def nodes: Seq[Node]

  /** output edges and referenced nodes from a node
    */
  def out: Node => Seq[(Edge, Node)]

  /** input edges and referenced nodes from a node
    */
  def in: Node => Seq[(Edge, Node)]

  /** sequence of triples in a graph
    */
  def triples: Seq[(Node, Edge, Node)]

  def neighbours(node: Node): Neighs_ = {
    val outs: Neighs_ = out(node).map { case (edge, node) => Direct(edge, node) }
    val ins: Neighs_ = in(node).map { case (edge, node) => Inverse(edge, node) }
    outs ++ ins
  }

}

object Graph {}
