package es.weso.depgraphs

import org.jgrapht.graph._
import es.weso.utils.internal.CollectionCompat.CollectionConverters._
import cats.implicits._
import alleycats.std.set._
import cats.effect._

case class Edge[Node, EdgeType](sub:Node, sup:Node, etype: EdgeType) {

  def show(showNode: Node => String, showEdge: EdgeType => String): String = 
    s"${showNode(sub)}${showEdge(etype)}${showNode(sup)}"

}

case class InheritanceJGraphT[Node,EdgeType](
 refGraph: Ref[IO,DirectedAcyclicGraph[Node,Edge[Node,EdgeType]]]
) extends Inheritance[Node,EdgeType] {

  override def clear: IO[Unit] = for {
    graph <- getGraph
  } yield {
   val edges: java.util.Set[Edge[Node,EdgeType]] = graph.edgeSet 
   graph.removeAllEdges(edges)
   val vertices: java.util.Set[Node] = graph.vertexSet()
   graph.removeAllVertices(vertices)
   ()
  }
   

  override def nodes: IO[Set[Node]] = for {
    graph <- getGraph
  } yield graph.vertexSet.asScala.toSet

  override def addNode(node: Node): IO[Unit] = for {
    graph <- getGraph
  } yield {
    graph.addVertex(node)
    ()
  }

  override def addInheritance(node1: Node, node2: Node, etype: EdgeType): IO[Unit] = 
   for {
     graph <- getGraph
   } yield {
    graph.addVertex(node1)
    graph.addVertex(node2)
    graph.addEdge(node1, node2, Edge(node1,node2,etype))
    ()
  }

  override def descendants(node: Node): IO[Set[Node]] = for {
    graph <- getGraph
  } yield if (graph.containsVertex(node)) {
    graph.getDescendants(node).asScala.toSet
  }
  else Set()

  private def collectDescendantsByEdgeType(node: Node, edgeType: EdgeType, visited: Set[Node], graph: DirectedAcyclicGraph[Node,Edge[Node,EdgeType]]): IO[Set[Node]] = {
    val incomingEdges = graph.incomingEdgesOf(node).asScala.toSet
    val nodesToCollect = incomingEdges.filter(edge => !visited.contains(edge.sub) && edge.etype == edgeType)
    val nodes = nodesToCollect.map(edge => edge.sub)
    nodes.map(node => 
       collectDescendantsByEdgeType(node, edgeType,visited ++ nodes, graph)
    )
    .sequence
    .map(_.flatten ++ nodes)
  }
    


  override def descendantsByEdgtype(node: Node, edgeType: EdgeType): IO[Set[Node]] = 
    getGraph.flatMap(graph => collectDescendantsByEdgeType(node, edgeType, Set(node), graph))

  override def ancestors(node: Node): IO[Set[Node]] = for {
    graph <- getGraph
  } yield if (graph.containsVertex(node)) 
      graph.getAncestors(node).asScala.toSet
  else Set()


  private def getGraph: IO[DirectedAcyclicGraph[Node,Edge[Node,EdgeType]]] =
    refGraph.get

  override def show(showNode: Node => String, showEdge: EdgeType => String): IO[String] = for {
    graph <- getGraph
  } yield graph.edgeSet().asScala.toList.map(edge => edge.show(showNode, showEdge)).mkString("\n")

}

object InheritanceJGraphT {

  def empty[Node,EdgeType]: IO[Inheritance[Node,EdgeType]] = for {
      refGraph <- 
        Ref.of[IO,DirectedAcyclicGraph[Node,Edge[Node,EdgeType]]](
          new DirectedAcyclicGraph[Node,Edge[Node,EdgeType]](classOf[Edge[Node,EdgeType]])
        ) 
  } yield InheritanceJGraphT(refGraph)

}