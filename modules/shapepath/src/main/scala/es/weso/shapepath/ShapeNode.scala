package es.weso.shapepath

// import es.weso.shex.Schema
import cats.Show
import cats.syntax.show._
import es.weso.shex.implicits.showShEx._
import es.weso.shex.{Shape, ShapeExpr, ShapeLabel, TripleExpr}
import es.weso.rdf.nodes.IRI

sealed abstract class ShapeNodeType 
case object TripleExprType extends ShapeNodeType
case object ShapeExprType extends ShapeNodeType
case object IRIType extends ShapeNodeType

sealed abstract class ShapeNode {
  def hasLabel(lbl: ShapeLabel):Boolean
  def getTripleExprByLabel(lbl: ShapeLabel, n: Option[Int]): Option[TripleExpr]
  def _type: ShapeNodeType
}
// case class SchemaItem(s: Schema) extends Item
case class ShapeExprItem(se: ShapeExpr) extends ShapeNode {
  override def hasLabel(otherLabel: ShapeLabel): Boolean = se.id match {
    case None => false
    case Some(lbl) => lbl == otherLabel
  }

  override def getTripleExprByLabel(lbl: ShapeLabel, n: Option[Int]): Option[TripleExpr] = se match {
    case s: Shape => s.expression match {
      case Some(te) => te.id match {
        case Some(teLbl) if te == teLbl => Some(te)
        case _ => None
      }
      case None => None
    }
    case _ => None
  }

  override def _type: ShapeNodeType = ShapeExprType

}

case class TripleExprItem(te: TripleExpr) extends ShapeNode {
  override def hasLabel(otherLabel: ShapeLabel): Boolean = te.id match {
    case None => false
    case Some(lbl) => lbl == otherLabel
  }

  override def getTripleExprByLabel(lbl: ShapeLabel, n: Option[Int]): Option[TripleExpr] = Some(te)

  override def _type: ShapeNodeType = TripleExprType

}

case class IRIItem(iri: IRI) extends ShapeNode {
  override def hasLabel(otherLabel: ShapeLabel): Boolean = false

  override def getTripleExprByLabel(lbl: ShapeLabel, n: Option[Int]): Option[TripleExpr] = None

  override def _type: ShapeNodeType = IRIType

}

object ShapeNode {

  implicit lazy val itemShow: Show[ShapeNode] = new Show[ShapeNode] {
    final def show(s: ShapeNode): String = s match {
      case ShapeExprItem(se) => se.show
      case TripleExprItem(te) => te.show
      case IRIItem(iri) => iri.show
    }
  }

}