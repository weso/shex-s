package es.weso.shex

import es.weso.rdf.nodes._

case class ResolvedTripleExpr(
    te: TripleExpr,
    source: Option[IRI]
)

object ResolvedTripleExpr {
  def apply(te: TripleExpr): ResolvedTripleExpr           = ResolvedTripleExpr(te, None)
  def apply(te: TripleExpr, iri: IRI): ResolvedTripleExpr = ResolvedTripleExpr(te, Some(iri))
}
