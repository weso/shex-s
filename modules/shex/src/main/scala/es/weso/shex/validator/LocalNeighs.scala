package es.weso.shex.validator

import es.weso.rdf.nodes.RDFNode
import cats._
// import cats.data._ 
import cats.implicits._

case class LocalNeighs(
    neighsMap: Map[RDFNode,Neighs]
) {

  def add(node: RDFNode, neighs: Neighs): LocalNeighs = 
    LocalNeighs(neighsMap.updated(node,neighs))

  def get(node: RDFNode): Option[Neighs] =
    neighsMap.get(node)

}

object LocalNeighs {
  implicit val localNeighsMonoid: Monoid[LocalNeighs] = new Monoid[LocalNeighs] {
    def combine(e1: LocalNeighs, e2: LocalNeighs): LocalNeighs = 
     LocalNeighs(e1.neighsMap |+| e2.neighsMap)

    def empty: LocalNeighs = LocalNeighs(Monoid[Map[RDFNode,Neighs]].empty)
  }

  implicit val localNeighsShow: Show[LocalNeighs] = new Show[LocalNeighs] {
    def show(e: LocalNeighs): String = "{" +
      e.neighsMap.map{ case (v,ls) => s"${v.show}->[${ls.map(_.show).mkString(",")}]}" } +
      "}"
  }

}