package es.weso.shex.validator

import es.weso.shex.ShapeLabel
import es.weso.shex.AbstractSchema

sealed abstract class WithDescendants {

  def isNoDescendants: Boolean = this match {
    case NoDescendants => true
    case _             => false
  }

  def addExcept(lbl: ShapeLabel) = this match {
    case NoDescendants     => NoDescendants 
    case FollowDescendants => FollowDescendants
  }

  def show(schema: AbstractSchema): String = this match {
    case NoDescendants => s"NoDescendants"
    case FollowDescendants => s"Follow"
  }

}
case object NoDescendants extends WithDescendants
case object FollowDescendants extends WithDescendants

object WithDescendants {
  val followDescendants = FollowDescendants
}
