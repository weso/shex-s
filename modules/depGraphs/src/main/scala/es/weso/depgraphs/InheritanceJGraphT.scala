package es.weso.depgraphs

import org.jgrapht.Graph
import org.jgrapht.alg.connectivity._
import org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm
import org.jgrapht.graph._
import es.weso.utils.internal.CollectionCompat.CollectionConverters._


trait InheritanceJGraphT[Node] extends Inheritance[Node] {

  case class Edge(sub:Node, sup:Node)

  val graph: Graph[Node, Edge] =
    new DefaultDirectedGraph[Node, Edge](classOf[Edge])


  private def removeAllEdges(): Boolean = {
    val edges: java.util.Set[Edge] = graph.edgeSet
    graph.removeAllEdges(edges)
  }

  override def empty: Inheritance[Node] = {
    removeAllEdges()
    this
  }

  override def nodes: Set[Node] = {
    graph.vertexSet.asScala.toSet
  }

  override def addNode(node: Node): Inheritance[Node] = {
    graph.addVertex(node)
    this
  }

  private def checkVertex(node: Node): Boolean = {
    // if (!graph.containsVertex(node))
    graph.addVertex(node)
  }

  override def addInheritance(node1: Node, node2: Node): Inheritance[Node] = {
    checkVertex(node1)
    checkVertex(node2)
    graph.addEdge(node1, node2, Edge(node1,node2))
    this
  }

  override def children(node: Node): List[Node] = {
    ???
  }

}