package es.weso.shex

import es.weso.rdf.nodes._

case class ResolvedShapeExpr(
    se: ShapeExpr,
    source: Option[IRI]
)

object ResolvedShapeExpr {
  def apply(se: ShapeExpr): ResolvedShapeExpr           = ResolvedShapeExpr(se, None)
  def apply(se: ShapeExpr, iri: IRI): ResolvedShapeExpr = ResolvedShapeExpr(se, Some(iri))
}
