package es.weso.shapepath

import cats.Show
import cats.syntax.show._
import es.weso.shex.ShapeLabel
import es.weso.shex.implicits.showShEx._

sealed abstract class ExprIndex
sealed abstract class ShapeExprIndex extends ExprIndex
case class IntShapeIndex(v: Int) extends ShapeExprIndex
case class ShapeLabelIndex(lbl: ShapeLabel) extends ShapeExprIndex

sealed abstract class TripleExprIndex extends ExprIndex
case class IntTripleExprIndex(v: Int) extends TripleExprIndex
case class LabelTripleExprIndex(lbl: ShapeLabel, n: Option[Int]) extends TripleExprIndex

object ExprIndex {
  implicit lazy val indexShow = new Show[ExprIndex] {
    final def show(e: ExprIndex): String = e match {
      case IntShapeIndex(i) => s"@$i"
      case ShapeLabelIndex(lbl) => s"@${lbl.show}"
      case IntTripleExprIndex(i) => s"$i"
      case LabelTripleExprIndex(lbl, None) => s"${lbl.show}"
      case LabelTripleExprIndex(lbl, Some(n)) => s"${lbl.show} $n"
    }
  }
}