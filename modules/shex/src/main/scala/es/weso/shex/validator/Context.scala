package es.weso.shex.validator

import cats._
import cats.implicits._
import es.weso.shex.VarTable
import es.weso.rdf.nodes.RDFNode
import es.weso.shex.ShapeLabel

case class Context(
  typing: ShapeTyping, 
  varTable: VarTable,
  localNeighs: LocalNeighs,
  visited: Set[ShapeLabel]
) {

  def addLocalNeighs(node: RDFNode, neighs: Neighs) = 
    this.copy(localNeighs = this.localNeighs.add(node, neighs))
  
  def updateTyping(f: ShapeTyping => ShapeTyping): Context = 
   this.copy(typing = f(this.typing))

  def addVisited(maybeLabel: Option[ShapeLabel]) = 
    maybeLabel match {
      case None => this
      case Some(label) => this.copy(visited = this.visited + label)
    }

}

object Context {
  implicit val ctxMonoid: Monoid[Context] = new Monoid[Context] {
    def combine(e1: Context, e2: Context): Context =
      Context(e1.typing |+| e2.typing, 
              e1.varTable |+| e2.varTable,
              e1.localNeighs |+| e2.localNeighs,
              e1.visited |+| e2.visited
             )

    def empty: Context = {
      Context(
        Monoid[ShapeTyping].empty, 
        Monoid[VarTable].empty, 
        Monoid[LocalNeighs].empty,
        Monoid[Set[ShapeLabel]].empty
        )
    }
  }

  def fromTyping(typing: ShapeTyping): Context = 
    Context(typing, 
      Monoid[VarTable].empty, 
      Monoid[LocalNeighs].empty,
      Monoid[Set[ShapeLabel]].empty
    )

}