package es.weso.wbmodel
import es.weso.rdf.nodes._
import org.wikidata.wdtk.datamodel.interfaces.{Value => WDTKValue}
import org.wikidata.wdtk.datamodel.implementation.ItemIdValueImpl

case class ItemId(
    id: String,
    iri: IRI,
    wdtkValue: Option[WDTKValue] = None
) extends EntityId {
  override def toString = s"$id"

  override def toWDTKValue: WDTKValue =
    wdtkValue match {
      case None =>
        val (name, base) = Utils.splitIri(iri)
        new ItemIdValueImpl(id, base)
      case Some(v) => v
    }
}
