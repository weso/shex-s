package es.weso.wbmodel
import cats.implicits._
import cats._
import es.weso.rdf.nodes._
import es.weso.wshex.ShapeLabel
import org.wikidata.wdtk.datamodel.interfaces.{
  DatatypeIdValue,
  QuantityValue => WDQuantityValue,
  SiteLink => WDTKSiteLink,
  Statement => WDStatement,
  StringValue => WDStringValue,
  Value => WDTKValue,
  _
}
import org.wikidata.wdtk.datamodel.interfaces.EntityDocument
import org.wikidata.wdtk.datamodel.interfaces.ItemDocument
import org.wikidata.wdtk.datamodel.interfaces.PropertyDocument

case class Item(
    itemId: ItemId,
    vertexId: VertexId,
    labels: Map[WBLang, String],
    descriptions: Map[WBLang, String],
    aliases: Map[WBLang, String],
    siteIri: String = Value.siteDefault,
    localStatements: List[LocalStatement],
    siteLinks: List[SiteLink],
    okShapes: Set[ShapeLabel] = Set()
//    wdtkValue: Option[WDTKValue] = None
) extends Entity {

  val entityId: EntityId = itemId
  def iri: IRI = IRI(siteIri + "/" + itemId.id)

  override def toString = s"${itemId.id}-${labels.get(WBLang("en")).getOrElse("")}@$vertexId"

  // override def toWDTKValue: WDTKValue =

  override def withLocalStatement(
      prec: PropertyRecord,
      literal: LiteralValue,
      qs: List[Qualifier] = List()
  ): Item =
    this.copy(
      localStatements = this.localStatements :+ LocalStatement(prec, literal, qs)
    )

  override def withOkShapes(shapes: Set[ShapeLabel]): Entity = this.copy(okShapes = shapes)

  override def addPropertyValues(pid: PropertyId, value: List[WDTKValue]): Entity = {
    println(s"AddPropertyValues: $pid not implemented yet")
    this
  }

  override def withStatement(s: WDStatement): Entity =
    this

}

object Item {

  def fromItemDocument(id: ItemDocument): Item = {
    val itemId = ItemId(id.getEntityId().getId(), IRI(id.getEntityId().getIri()))
    Item(
      itemId,
      VertexId(0L),
      Map(),
      Map(),
      Map(),
      id.getEntityId().getSiteIri(),
      List(),
      List(),
      Set()
    )
  }
}
