package es.weso.wbmodel
import es.weso.rdf.nodes.IRI
import cats._
import org.wikidata.wdtk.datamodel.implementation._
import org.wikidata.wdtk.datamodel.interfaces.{
  PropertyIdValue => WDTKPropertyIdValue,
  Value => WDTKValue,
  _
}

case class PropertyId(
    id: String,
    iri: IRI,
    wdtkValue: Option[WDTKPropertyIdValue] = None
) extends EntityId {
  override def toString = s"$id"

  override def toWDTKValue: WDTKPropertyIdValue =
    wdtkValue match {
      case None =>
        val (name, base) = Utils.splitIri(iri)
        new PropertyIdValueImpl(id, base)
      case Some(v) => v
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

  def fromPropertyIdValue(pidValue: WDTKPropertyIdValue): PropertyId =
    PropertyId(pidValue.getId(), IRI(pidValue.getIri()))
}
