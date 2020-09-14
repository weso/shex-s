package es.weso.shex.normalized
import es.weso.shex._
import cats._
import cats.implicits._
import es.weso.shex.implicits.showShEx._


case class Constraint(shape: Option[ShapeExpr],
                      hasExtra: Boolean,
                      card: Cardinality,
                      as: Option[List[Annotation]],
                      tc: TripleConstraint // Reference to original constraint
                      ) 

object Constraint {

    implicit lazy val showConstraint: Show[Constraint] = new Show[Constraint] {
    final def show(c: Constraint): String = {
      s"${c.shape.fold(".")(_.show)}${if (c.hasExtra) " EXTRA" else ""}${c.card.show}"
    }
  }
}                      
