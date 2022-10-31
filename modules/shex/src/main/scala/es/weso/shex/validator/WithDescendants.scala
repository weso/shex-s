package es.weso.shex.validator

sealed abstract class WithDescendants {

  def isNoDescendants: Boolean = this match {
    case NoDescendants => true
    case _             => false
  }

  def isFollowDescendants: Boolean = this match {
    case FollowDescendants => true
    case _ => false
  }

  def show: String = this match {
    case NoDescendants => s"NoDescendants"
    case FollowDescendants => s"Follow"
  }

}
case object NoDescendants extends WithDescendants
case object FollowDescendants extends WithDescendants

object WithDescendants {
  val followDescendants = FollowDescendants
}

