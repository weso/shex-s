package es.weso.shex

import es.weso.rdf.nodes._

case class ResolvedShapeExpr(
    se: ShapeExpr,
    source: Option[IRI],
    descendants: Set[ShapeLabel],
    paths: List[Path]
) {
  def withDescendants(ds: Set[ShapeLabel]): ResolvedShapeExpr =
    this.copy(descendants = ds)

  def withPaths(ps: List[Path]): ResolvedShapeExpr =
    this.copy(paths = ps)

}

object ResolvedShapeExpr {
  def apply(se: ShapeExpr): ResolvedShapeExpr = ResolvedShapeExpr(se, None, Set(), List())
  def apply(se: ShapeExpr, iri: IRI): ResolvedShapeExpr =
    ResolvedShapeExpr(se, Some(iri), Set(), List())
}
