package es.weso.depgraphs

import org.jgrapht.graph._
import es.weso.utils.internal.CollectionCompat.CollectionConverters._
import cats.effect._
import cats.effect.concurrent._

case class Edge[Node](sub:Node, sup:Node)

case class InheritanceJGraphT[Node](refGraph: Ref[IO,DirectedAcyclicGraph[Node,Edge[Node]]]) extends Inheritance[Node] {

  override def clear: IO[Unit] = for {
    graph <- getGraph
  } yield {
   val edges: java.util.Set[Edge[Node]] = graph.edgeSet 
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

  override def addInheritance(node1: Node, node2: Node): IO[Unit] = 
   for {
     graph <- getGraph
   } yield {
    graph.addVertex(node1)
    graph.addVertex(node2)
    graph.addEdge(node1, node2, Edge(node1,node2))
    ()
  }

  override def descendants(node: Node): IO[Set[Node]] = for {
    graph <- getGraph
  } yield graph.getDescendants(node).asScala.toSet

  override def ancestors(node: Node): IO[Set[Node]] = for {
    graph <- getGraph
  } yield graph.getAncestors(node).asScala.toSet


  def getGraph: IO[DirectedAcyclicGraph[Node,Edge[Node]]] =
    refGraph.get

}

object InheritanceJGraphT {

  def empty[Node]: IO[Inheritance[Node]] = for {
      refGraph <- 
        Ref.of[IO,DirectedAcyclicGraph[Node,Edge[Node]]](
          new DirectedAcyclicGraph[Node,Edge[Node]](classOf[Edge[Node]])
        ) 
  } yield InheritanceJGraphT(refGraph)

}