package es.weso.shex

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
import cats._
import cats.data._
import cats.implicits._
import es.weso.utils.EitherUtils._
import es.weso.utils.OptionListUtils._
import es.weso.shex.extend.Extend


trait AllPaths extends Extend {

  /** Return all the paths that are mentioned in a shape expression
    * It includes also the paths it extends and the paths from its descendants
    * @param schema Schema to which the shape belongs, it is needed to resolve references to other shapes
    * @return Set of paths or error in case the shape is not well defined (may have bad references)
    */
  def allPaths(se: ShapeExpr, schema: AbstractSchema): Either[String, Set[Path]] = {
    extendCheckingVisited[ShapeExpr, List[Path], String, ShapeLabel](se, finder(schema), extend, getPaths(schema))
      .map(_.getOrElse(List()))
      .map(_.toSet)
  }

  private def finder(schema: AbstractSchema)(lbl: ShapeLabel): Either[String, ShapeExpr] =
    schema.getShape(lbl)

  private def getPaths(schema: AbstractSchema)(s: ShapeExpr): Option[List[Path]] = {
      def cnv[A, B](e: Either[B, Set[A]]): Option[List[A]] = e.fold(_ => none, _.toList.some)
      s match {
        case s: Shape      => s.expression.map(_.paths(schema).toList).getOrElse(List()).some
        case sd: ShapeDecl => cnv(sd.paths(schema))
        case sa: ShapeAnd  => cnv(sa.paths(schema))
        case so: ShapeOr   => cnv(so.paths(schema))
        case sn: ShapeNot  => cnv(sn.paths(schema))
        case _             => List().some
      }
    }

  private def extend(se: ShapeExpr): Option[List[ShapeLabel]] = se match {
    case s: Shape => s._extends
    case sd: ShapeDecl => extend(sd.shapeExpr)
    case sa: ShapeAnd => sa.shapeExprs.map(extend(_)).sequence.map(_.flatten)
    case so: ShapeOr => so.shapeExprs.map(extend(_)).sequence.map(_.flatten)
    case sn: ShapeNot => extend(sn.shapeExpr)
    case sr: ShapeRef => none
    case nc: NodeConstraint => none
    case se: ShapeExternal => none
  }


}