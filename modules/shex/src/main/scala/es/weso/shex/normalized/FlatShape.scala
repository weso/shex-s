package es.weso.shex.normalized

import cats.implicits._
import es.weso.rdf.nodes.IRI
import es.weso.shex._
import cats._
import cats.implicits._
import es.weso.shex.implicits.showShEx._

/**
  * A flat shape consists of a list of slots where each slot is formed by a path and a constraint.
  * It has no repeated properties
  * It can be represented as a map from a path to a constraint.
  * @param slots a vector of pairs (Path, Constraint)
  */
case class FlatShape(id: Option[ShapeLabel], slots: Map[Path, Constraint], closed: Boolean) {
  lazy val paths: Set[Path]               = slots.keySet
  lazy val preds: Set[IRI]                = paths.collect { case Direct(p) => p }
  lazy val hasRepeatedProperties: Boolean = false
}

object FlatShape {

  def fromShape(shape: Shape, schema: AbstractSchema): Either[String, FlatShape] = {
    val empty: Map[Path, Constraint] = Map()
    for {
      cs <- shape.expression.fold(empty.asRight[String])(
        flattenTripleExpr(_, empty, shape.extraPaths, schema)
      )
    } yield FlatShape(shape.id, cs, shape.isClosed)
  }

  private def flattenTripleExpr(
      te: TripleExpr,
      cs: Map[Path, Constraint],
      extraPaths: List[Path],
      schema: AbstractSchema
  ): Either[String, Map[Path, Constraint]] = te match {
    case _: Expr      => Left(s"Contains an expr")
    case _: Inclusion => Left(s"Contains an inclusion")
    case eo: EachOf if !Cardinality.isDefault(eo.min,eo.max) => Left(s"Each of contains groupings")
    case _ if te.hasSemActs => Left(s"TripleExpr contains semantic actions")
    case eo: EachOf => {
      val zero = cs.asRight[String]
      def cmb(current: Either[String, Map[Path, Constraint]], te: TripleExpr): Either[String, Map[Path, Constraint]] =
        for {
          cs  <- current
          cs1 <- flattenTripleExpr(te, cs, extraPaths, schema)
        } yield cs1
      eo.expressions.foldLeft(zero)(cmb)
    }
    case _: OneOf => Left(s"Contains a oneOf")
    case tc: TripleConstraint =>
      if (cs.keySet contains tc.path) Left(s"Repeated properties: ${tc.path}")
      else
        tc.valueExpr match {
          case None =>
            cs.updated(
                tc.path,
                Constraint(tc.valueExpr, extraPaths contains tc.path, Cardinality(tc.min, tc.max), tc.annotations, tc)
              )
              .asRight[String]
          case Some(se) =>
            se match {
              case _ if (!se.hasNoReference(schema)) => s"${se.show} contains a reference".asLeft
              case _ if (!se.isSimple)       => s"${se.show} is not simple".asLeft
              case _ =>
                cs.updated(
                    tc.path,
                    Constraint(
                      tc.valueExpr,
                      extraPaths contains tc.path,
                      Cardinality(tc.min, tc.max),
                      tc.annotations,
                      tc
                    )
                  )
                  .asRight[String]
            }
        }
  }

  implicit lazy val showFlatShape: Show[FlatShape] = new Show[FlatShape] {
    final def show(c: FlatShape): String = {
      s"FlatShape(${c.id.map(_.toRDFNode.show).getOrElse("?")}), closed: ${c.closed}\nSlots: ${c.slots.map(showSlot).mkString("\n")}"
    }

    private def showSlot(pair: (Path, Constraint)): String = {
      val (path, c) = pair
      s"${path.show} -> ${c.show}"
    }
  }

}
