package es.weso.shapepath
import cats.Show
import cats.syntax.show._
import es.weso.shex.implicits.showShEx._
import es.weso.shex.ShapeLabel

sealed abstract class Step
case class ExprStep(maybeContext: Option[Context], exprIndex: ExprIndex) extends Step
case class ContextStep(context: Context) extends Step



object Step {

  implicit lazy val stepShow = new Show[Step] {
    final def show(s: Step): String = s match {
      case ExprStep(None,index) => s"${index.show}"
      case ExprStep(Some(ctx),index) => s"${ctx.show} ${index.show}"
      case ContextStep(ctx) => s"${ctx.show}"
    }
  }

}