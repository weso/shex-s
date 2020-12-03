package es.weso.shex.validator

import cats.Show
import cats.implicits._
import es.weso.shex.{SemAct, ShapeExpr}
import io.circe._
import io.circe.syntax._

private[validator] sealed trait CheckExpr

private[validator] case class Pos(se: ShapeExpr, semActs: Option[List[SemAct]]) extends CheckExpr

private[validator] case class Neg(se: ShapeExpr, semActs: Option[List[SemAct]]) extends CheckExpr

private[validator] object CheckExpr {
  import es.weso.shex.implicits.showShEx._

  implicit lazy val showCheckExpr = new Show[CheckExpr] {
    override def show(ce: CheckExpr): String = {
      ce match {
        case Pos(se,semActs) => se.show
        case Neg(se,semActs) => "!" + se.show
      }
    }
  }

   implicit val checkExprEncoder: Encoder[CheckExpr] = new Encoder[CheckExpr] {
    final def apply(v: CheckExpr): Json = 
      v.show.asJson
  }


}

