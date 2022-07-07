package es.weso.shapepath
import cats.Show

sealed abstract class Axis extends Product with Serializable {
  def symbol: String
}

case object Child extends Axis {
  override val symbol: String = "child"
}
case object Descendant extends Axis {
  override val symbol: String = "descendant"
}
case object NestedShapeExpr extends Axis {
  override val symbol: String = "nested-or-self-shapeExpr"
}
case object NestedTripleExpr extends Axis {
  override val symbol: String = "nested-or-self-tripleConstraint"
}

object Axis {
  implicit lazy val axisShow: Show[Axis] = new Show[Axis] {
    final def show(s: Axis): String = s.symbol
  }
}
