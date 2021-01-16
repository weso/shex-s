package es.weso.shapepath

// import es.weso.shex.Schema
import cats.Show
import cats.syntax.show._
import es.weso.shex.implicits.showShEx._
import es.weso.shex.{Shape, ShapeExpr, ShapeLabel, TripleExpr}

sealed abstract class Item {
  def hasLabel(lbl: ShapeLabel):Boolean
  def getTripleExprLabel(lbl: ShapeLabel, n: Option[Int]): Option[TripleExpr]
}
// case class SchemaItem(s: Schema) extends Item
case class ShapeExprItem(se: ShapeExpr) extends Item {
  override def hasLabel(otherLabel: ShapeLabel): Boolean = se.id match {
    case None => false
    case Some(lbl) => lbl == otherLabel
  }

  override def getTripleExprLabel(lbl: ShapeLabel, n: Option[Int]): Option[TripleExpr] = se match {
    case s: Shape => s.expression match {
      case Some(te) => te.id match {
        case Some(teLbl) if te == teLbl => Some(te)
        case _ => None
      }
      case None => None
    }
    case _ => None
  }

}

case class TripleExprItem(te: TripleExpr) extends Item {
  override def hasLabel(otherLabel: ShapeLabel): Boolean = te.id match {
    case None => false
    case Some(lbl) => lbl == otherLabel
  }

  override def getTripleExprLabel(lbl: ShapeLabel, n: Option[Int]): Option[TripleExpr] = ???

}

object Item {

  implicit lazy val itemShow: Show[Item] = new Show[Item] {
    final def show(s: Item): String = s match {
      case ShapeExprItem(se) => se.show
      case TripleExprItem(te) => te.show
    }
  }

}