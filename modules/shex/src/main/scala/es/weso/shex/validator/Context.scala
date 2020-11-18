package es.weso.shex.validator

import cats._
import cats.implicits._
import es.weso.shex.VarTable
import es.weso.rdf.nodes.RDFNode

case class Context(
  typing: ShapeTyping, 
  varTable: VarTable,
  localNeighs: LocalNeighs
) {

  def addLocalNeighs(node: RDFNode, neighs: Neighs) = 
    this.copy(localNeighs = this.localNeighs.add(node, neighs))
  
  def updateTyping(f: ShapeTyping => ShapeTyping): Context = 
   this.copy(typing = f(this.typing))

}

object Context {
  implicit val ctxMonoid: Monoid[Context] = new Monoid[Context] {
    def combine(e1: Context, e2: Context): Context =
      Context(e1.typing |+| e2.typing, 
              e1.varTable |+| e2.varTable,
              e1.localNeighs |+| e2.localNeighs
             )

    def empty: Context = {
      Context(Monoid[ShapeTyping].empty, Monoid[VarTable].empty, Monoid[LocalNeighs].empty)
    }
  }

  def fromTyping(typing: ShapeTyping): Context = 
    Context(typing, Monoid[VarTable].empty, Monoid[LocalNeighs].empty)

}