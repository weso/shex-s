package es.weso.shex.validator

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
import cats._
import cats.data._
import cats.implicits._
import es.weso.utils.EitherUtils._
import es.weso.utils.OptionListUtils._
import es.weso.shex.extend.ExtendM
import es.weso.shex._
import es.weso.shex.validator.PartitionUtils._
import cats.effect._
import es.weso.shex.validator.ShExError._

trait AvailableShapeExprPaths extends ExtendM {

  /** Return all the pairs (se,paths) where se is a ShapeExpr and paths are the available paths for that ShapeExpr
    *  It includes also the paths it extends and the paths from its descendants
    * @param s shape
    * @param schema Schema to which the shape belongs, it is needed to resolve references to other shapes
    * @return Set of paths or error in case the shape is not well defined (may have bad references)
    */

  def getAvailableShapeExprsPaths(
      s: Shape,
      schema: ResolvedSchema,
      parent: Option[ShapeLabel]
  ): IO[List[(ShapeExpr, Available[Path])]] = {

    def finder(lbl: ShapeLabel): IO[ShapeExpr] =
      schema
        .getShape(lbl)
        .leftMap(ShExError.LabelNotFound(lbl, _, schema.labels))
        .fold(
          err => IO.raiseError(err),
          v => IO.pure(v)
        )

    def extend(se: ShapeExpr): IO[List[ShapeLabel]] = se match {
      case shape: Shape =>
        IO.pure(shape._extends.getOrElse(List()))
      /* case sd: ShapeDecl =>
        extend(sd.shapeExpr) */
      case _ => IO(List())
    }

    def first(se: ShapeExpr): IO[List[(ShapeExpr, Available[Path])]] =
      rest(se).flatMap(rs =>
        parent match {
          case None      => rs.pure
          case Some(lbl) => rs.pure // TODO: Nothing by now
        }
      )

    def getSEsLabels(ds: List[ShapeLabel]): IO[List[ShapeExpr]] =
      ds.map(finder(_)).sequence

    def getSEPathsSEs(ses: List[ShapeExpr]): IO[List[(ShapeExpr, Available[Path])]] =
      ses.map(rest(_)).sequence.map(_.flatten)

    def rest(se: ShapeExpr): IO[List[(ShapeExpr, Available[Path])]] =
      getAvailablePathsSE(se).map(List(_))

    def getAvailablePathsSE(se: ShapeExpr): IO[(ShapeExpr, Available[Path])] = se match {
      case sd: ShapeDecl => getPaths(sd.shapeExpr).map(ps => (sd.shapeExpr, ps))
      case _             => getPaths(se).map(ps => (se, ps))
    }

    def getPaths(se: ShapeExpr): IO[Available[Path]] =
      se.paths(schema)
        .fold(
          err => IO.raiseError(StringError(err)),
          Available(_).pure
        )

    extendCheckingVisitedM1[
      ShapeExpr,
      List[(ShapeExpr, Available[Path])],
      ShapeLabel,
      IO
    ](s, finder, extend, first, rest)

  }

}
