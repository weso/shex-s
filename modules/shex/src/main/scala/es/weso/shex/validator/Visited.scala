package es.weso.shex.validator

import es.weso.shex.ShapeLabel
import cats.implicits._
import es.weso.shex.AbstractSchema
import es.weso.rdf.nodes.RDFNode
import es.weso.utils.internal.CollectionCompat._

case class Visited(m: Map[RDFNode, Set[ShapeLabel]]) extends AnyVal {
  def contains(node: RDFNode, lbl: ShapeLabel): Boolean = m.get(node).map(_.contains(lbl)).getOrElse(false)

  def add(node: RDFNode, lbl: ShapeLabel): Visited =
    this.copy(m = m.updated(node, m.get(node).map(vs => vs + lbl).getOrElse(Set(lbl))))

  def remove(node: RDFNode, lbl: ShapeLabel): Visited =
    this.copy(m =
      updatedWith(m)(node)(mvs =>
        mvs match {
          case None     => none[Set[ShapeLabel]]
          case Some(vs) => Some(vs - lbl)
        }
      )
    )

  def show(schema: AbstractSchema): String =
    s"Visited: ${m.map { case (n, vs) => s"${n.show}->${vs.map(lbl => schema.qualify(lbl)).mkString(",")}" }.mkString(" | ")}"
}

object Visited {
  def empty: Visited = Visited(Map())
}
