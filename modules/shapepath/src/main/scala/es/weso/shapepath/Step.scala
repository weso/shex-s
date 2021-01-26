package es.weso.shapepath
import cats.Show
import cats.syntax.show._
import es.weso.rdf.PrefixMap
import java.awt.Label
import es.weso.rdf.nodes.IRI
import es.weso.shex.IRILabel

sealed abstract class Step {
  def showQualify(pm: PrefixMap): String
  def addPredicates(ps: List[Predicate]): Step
}

case class NodeTestStep(
  axis: Axis, 
  nodeTest: NodeTest,
  predicates: List[Predicate]) extends Step {

  override def showQualify(pm: PrefixMap): String = 
    axis.toString +
    nodeTest.showQualify(pm)

  override def addPredicates(ps: List[Predicate]) = 
   this.copy(predicates = this.predicates ++ ps)
   

}

case class ExprStep(
   maybeContext: Option[Context], 
   exprIndex: ExprIndex,
   predicates: List[Predicate]
  ) extends Step {

  override def showQualify(pm: PrefixMap): String = 
    maybeContext.map(_.symbol).getOrElse("") +
    exprIndex.showQualify(pm)

  override def addPredicates(ps: List[Predicate]) = 
   this.copy(predicates = this.predicates ++ ps)

}

object Step {

  implicit lazy val stepShow = new Show[Step] {
    final def show(s: Step): String = s match {
      case ExprStep(None,index, _) => s"${index.show}"
      case ExprStep(Some(ctx),index, _) => s"${ctx.show} ${index.show}"
    }
  }

  def fromIRI(pred: IRI): ExprStep = 
   ExprStep(None,LabelTripleExprIndex(IRILabel(pred), None), List())

  def fromShapeLabel(lbl: IRI): ExprStep = 
   ExprStep(None,ShapeLabelIndex(IRILabel(lbl)), List())

  def mkStep(maybeAxis: Option[Axis], nodeTest: NodeTest): Step = {
    val axis = maybeAxis.getOrElse(Child)
    NodeTestStep(axis, nodeTest, List())
  }
   

}