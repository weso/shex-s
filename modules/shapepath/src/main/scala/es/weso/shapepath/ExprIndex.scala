package es.weso.shapepath

import cats.Show
import cats.syntax.show._
import es.weso.shex.ShapeLabel
import es.weso.shex.implicits.showShEx._
import es.weso.rdf.PrefixMap

sealed abstract class ExprIndex {
  def showQualify(pm: PrefixMap): String
}
sealed abstract class ShapeExprIndex extends ExprIndex {
  override def showQualify(pm: PrefixMap): String
}
case class IntShapeIndex(v: Int) extends ShapeExprIndex {
  override def showQualify(pm: PrefixMap): String = v.toString
}
case class ShapeLabelIndex(lbl: ShapeLabel) extends ShapeExprIndex {
  override def showQualify(pm: PrefixMap): String = 
    lbl.showQualify(pm)
}

sealed abstract class TripleExprIndex extends ExprIndex {
  override def showQualify(pm: PrefixMap): String
}
case class IntTripleExprIndex(v: Int) extends TripleExprIndex {
  override def showQualify(pm: PrefixMap): String = v.toString
}
case class LabelTripleExprIndex(lbl: ShapeLabel, n: Option[Int]) extends TripleExprIndex {
  override def showQualify(pm: PrefixMap): String = 
    lbl.showQualify(pm) + n.map(_.toString).getOrElse("")
}

object ExprIndex {
  implicit lazy val indexShow: Show[ExprIndex] = new Show[ExprIndex] {
    final def show(e: ExprIndex): String = e match {
      case IntShapeIndex(i) => s"@$i"
      case ShapeLabelIndex(lbl) => s"@${lbl.show}"
      case IntTripleExprIndex(i) => s"$i"
      case LabelTripleExprIndex(lbl, None) => s"${lbl.show}"
      case LabelTripleExprIndex(lbl, Some(n)) => s"${lbl.show} $n"
    }
  }
}