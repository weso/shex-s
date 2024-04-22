package es.weso.shex

import cats.implicits._
import es.weso.rdf.nodes._
import scala.util._
import cats.effect.IO
import es.weso.depgraphs.Inheritance
import es.weso.depgraphs.InheritanceJGraphT
import es.weso.rdf.locations.Location
import es.weso.utils.VerboseLevel

object InheritanceGraph {

  private def addLs(
      g: Inheritance[ShapeLabel, ShapesRelation],
      ls: List[ShapeLabel],
      sub: ShapeLabel,
      rel: ShapesRelation
  ) = {
    def cmb(c: Unit, e: ShapeLabel): IO[Unit] =
      g.addInheritance(sub, e, rel)
    ls.foldM(())(cmb)
  }

  private def addExtendsRestricts(
      g: Inheritance[ShapeLabel, ShapesRelation],
      sub: ShapeLabel,
      shape: Shape
  ): IO[Unit] =
    (shape._extends, shape.restricts) match {
      case (None, None)     => ().pure[IO]
      case (Some(es), None) => addLs(g, es, sub, Extends)
      case (None, Some(rs)) => addLs(g, rs, sub, Restricts)
      case (Some(es), Some(rs)) =>
        addLs(g, es, sub, Extends) *>
          addLs(g, rs, sub, Restricts)
    }

  private def showSE(se: ShapeExpr): String = se match {
    case and: ShapeAnd => "AND"
    case s: Shape      => "Shape"
    case sd: ShapeDecl => "ShapeDecl"
    case sr: ShapeRef  => s"ShapeRef(${sr.reference.toRDFNode.show})"
    case _             => "other"
  }

  // TODO: Check possible infinite loop when shape exprs contain themselves...
  private def addShapeExpr(
      g: Inheritance[ShapeLabel, ShapesRelation],
      sub: ShapeLabel,
      se: ShapeExpr,
      verbose: VerboseLevel
  ): IO[Unit] =
    se match {
      case s: Shape => addExtendsRestricts(g, sub, s)
      /*      case s: ShapeAnd =>
        s.shapeExprs.foldM(()){
          case (_, se) => addShapeExpr(g, sub, se, verbose)
        } */
      case sd: ShapeDecl => addShapeExpr(g, sub, sd.shapeExpr, verbose)
      case sr: ShapeRef  => g.addInheritance(sub, sr.reference, References)
      case _             => ().pure[IO]
    }

  private def addPair(g: Inheritance[ShapeLabel, ShapesRelation], verboseLevel: VerboseLevel)(
      u: Unit,
      pair: (ShapeLabel, ResolvedShapeExpr)
  ): IO[Unit] = {
    val (shapeLabel, resolvedShapeExpr) = pair
    addShapeExpr(g, shapeLabel, resolvedShapeExpr.se, verboseLevel)
  }

  def mkInheritanceGraph(
      m: Map[ShapeLabel, ResolvedShapeExpr],
      verboseLevel: VerboseLevel
  ): IO[Inheritance[ShapeLabel, ShapesRelation]] = for {
    g <- InheritanceJGraphT.empty[ShapeLabel, ShapesRelation]
    _ <- m.toList.foldM(())(addPair(g, verboseLevel))
  } yield g

  // TODO: Should we use a resource better?
  def empty: IO[Inheritance[ShapeLabel, ShapesRelation]] =
    InheritanceJGraphT.empty[ShapeLabel, ShapesRelation]

}
