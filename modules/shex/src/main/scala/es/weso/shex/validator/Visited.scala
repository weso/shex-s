package es.weso.shex.validator

import es.weso.shex.ShapeLabel
import cats.implicits._
import es.weso.shex.AbstractSchema

case class Visited(vs: Set[ShapeLabel]) extends AnyVal {
  def contains(lbl: ShapeLabel): Boolean = vs.contains(lbl)

  def add(lbl: ShapeLabel): Visited = this.copy(vs = vs + lbl)

  def show(schema: AbstractSchema): String = s"Visited: ${vs.map(schema.qualify(_)).mkString(",")}"
}

object Visited {
  def empty: Visited = Visited(Set())

}
