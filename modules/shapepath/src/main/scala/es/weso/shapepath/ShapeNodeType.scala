package es.weso.shapepath

import cats.Show

sealed abstract class ContextType extends Product with Serializable {
  def symbol: String
}
case object ShapeAndType extends ContextType {
  override def symbol = "ShapeAnd"

}
case object ShapeOrType extends ContextType {
  override def symbol = "ShapeOr"
}
case object ShapeNotType extends ContextType {
  override def symbol = "ShapeNot"
}
case object NodeConstraintType extends ContextType {
  override def symbol = "NodeConstraint"
}
case object ShapeType extends ContextType {
  override def symbol = "Shape"
}
case object EachOfType extends ContextType {
  override def symbol = "EachOf"
}
case object OneOfType extends ContextType {
  override def symbol = "OneOf"
}
case object TripleConstraintType extends ContextType {
  override def symbol = "TripleConstraint"
}

object ContextType {
  implicit lazy val shapetypeShow: Show[ContextType] = new Show[ContextType] {
    final def show(c: ContextType): String = c.symbol
  }
}