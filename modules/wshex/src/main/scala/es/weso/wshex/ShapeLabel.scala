package es.weso.wshex

import es.weso.rdf.nodes._
import cats._

sealed abstract class ShapeLabel extends Serializable with Product {
  def name: String
  override def toString = name
}
case object Start extends ShapeLabel {
  override def name = "Start"
}
case class IRILabel(iri: IRI) extends ShapeLabel {
  override def name = iri.getLexicalForm
}
case class BNodeLabel(bnode: BNode) extends ShapeLabel {
  override def name = bnode.getLexicalForm
}


object ShapeLabel {
  implicit val showShapeLabel: Show[ShapeLabel] = Show.show(lbl => lbl match {
    case Start => "Start"
    case IRILabel(iri) => iri.getLexicalForm
    case BNodeLabel(bnode) => bnode.getLexicalForm
  })
  implicit val orderingByName: Ordering[ShapeLabel] = new Ordering[ShapeLabel] {
    def compare(a:ShapeLabel, b:ShapeLabel) = (a,b) match {
      case (Start,Start) => 0
      case (Start, _) => -1
      case (IRILabel(iri), Start) => 1
      case (IRILabel(iri1), IRILabel(iri2)) => iri1.getLexicalForm.compare(iri2.getLexicalForm)
      case (IRILabel(_), BNodeLabel(_) ) => -1
      case (BNodeLabel(_), Start) => 1
      case (BNodeLabel(_), IRILabel(_)) => 1
      case (BNodeLabel(b1), BNodeLabel(b2)) => b1.getLexicalForm.compare(b2.getLexicalForm)
    }
   }
}   
    
