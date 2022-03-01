package es.weso.shex.validator

import es.weso.shex.ShapeLabel
import es.weso.shex.AbstractSchema

sealed abstract class WithDescendants {

  def isNoDescendants: Boolean = this match {
    case NoDescendants => true
    case _             => false
  }

  def addExcept(lbl: ShapeLabel) = this match {
    case NoDescendants         => NoDescendants // FollowDescendants(Set(lbl))
    case FollowDescendants(ls) => FollowDescendants(ls + lbl)
  }

  def show(schema: AbstractSchema): String = this match {
    case NoDescendants => s"NoDescendants"
    case FollowDescendants(ls) =>
      s"Follow ${if (ls.isEmpty) "" else s"except [${ls.map(schema.qualify(_)).mkString(",")}]"}"
  }

}
case object NoDescendants                             extends WithDescendants
case class FollowDescendants(except: Set[ShapeLabel]) extends WithDescendants

object WithDescendants {
  val followDescendants = FollowDescendants(Set())
}
