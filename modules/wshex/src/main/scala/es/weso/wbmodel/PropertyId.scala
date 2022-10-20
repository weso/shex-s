package es.weso.wbmodel
import es.weso.rdf.nodes.IRI
import cats._
import org.wikidata.wdtk.datamodel.implementation._
import org.wikidata.wdtk.datamodel.interfaces._

case class PropertyId(
    id: String,
    iri: IRI
) extends EntityId {
  override def toString = s"$id"

  def toValue: PropertyIdValue = {
    val (name,base) = Utils.splitIri(iri)
    new PropertyIdValueImpl(id, base)
  }
}

object PropertyId {

  implicit val showPropertyId: Show[PropertyId] = Show.show(p => p.id.toString)
  implicit val orderingById: Ordering[PropertyId] = Ordering.by(_.id)
  def fromIRI(iri: IRI): PropertyId = {
    val (name, base) = Utils.splitIri(iri)
    PropertyId(name, iri)
  }
  def fromNumber(n: Int, baseProperty: IRI): PropertyId =
    PropertyId(s"P$n", baseProperty + s"P$n")

}
