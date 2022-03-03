package es.weso.shex

sealed abstract class ShapesRelation {
  val name: String
}
case object Extends extends ShapesRelation {
  override val name = "extends"
}
case object References extends ShapesRelation {
  override val name = "refers"
}
case object Restricts extends ShapesRelation {
  override val name = "restricts"
}
