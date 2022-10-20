package es.weso.wbmodel
import es.weso.rdf.nodes._

case class ItemId(id: String, iri: IRI) extends EntityId {
  override def toString = s"$id"
}
