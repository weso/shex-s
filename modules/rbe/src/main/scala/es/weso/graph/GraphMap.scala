package es.weso.graph

/** Implementation of graph as a map
  */
case class GraphMap[Edge, Node](
  m: Map[Node, Seq[(Edge, Node)]]
  ) extends Graph[Edge, Node] {
  def nodes = m.keys.toSeq
  def triples =
    m.map { case (x, out) => out.map { case (e, o) => (x, e, o) } }.flatten.toSeq

  def out = { n =>
    m.get(n).getOrElse(Seq())
  }

  def in = { n =>
    type Pairs = Seq[(Edge, Node)]
    val empty: Pairs = Seq()
    def next(rest: Pairs, current: (Node, Pairs)): Pairs =
      findNodePairs(n, current) ++ rest
    def findNodePairs(node: Node, pairs: (Node, Pairs)): Pairs = {
      val origin = pairs._1
      def filterPair(rest: Pairs, current: (Edge, Node)): Pairs =
        if (current._2 == node) (current._1, origin) +: rest
        else rest
      pairs._2.foldLeft(empty)(filterPair)
    }
    m.foldLeft(empty)(next)
  }

}
