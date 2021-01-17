package es.weso.shapepath

import cats.Show

sealed abstract class Context {
  def symbol: String
}
case object ShapeAndCtx extends Context {
  override def symbol = "ShapeAnd"

}
case object ShapeOrCtx extends Context {
  override def symbol = "ShapeOr"
}
case object ShapeNotCtx extends Context {
  override def symbol = "ShapeNot"
}
case object NodeConstraintCtx extends Context {
  override def symbol = "NodeConstraint"
}
case object ShapeCtx extends Context {
  override def symbol = "Shape"
}
case object EachOfCtx extends Context {
  override def symbol = "EachOf"
}
case object OneOfCtx extends Context {
  override def symbol = "OneOf"
}
case object TripleConstraintCtx extends Context {
  override def symbol = "TripleConstraint"
}

object Context {
  implicit lazy val contextShow = new Show[Context] {
    final def show(c: Context): String = c.symbol
  }
}