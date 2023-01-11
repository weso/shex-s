package es.weso.wbmodel

import cats.implicits._
import cats._
import es.weso.rdf.nodes.{Lang => _, _}
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

case class Property(
    propertyId: PropertyId,
    vertexId: VertexId,
    labels: Map[WBLang, String],
    descriptions: Map[WBLang, String],
    aliases: Map[WBLang, String],
    siteIri: String = Value.siteDefault,
    localStatements: List[LocalStatement] = List(),
    datatype: Datatype = Datatype.defaultDatatype,
    okShapes: Set[ShapeLabel] = Set(),
    wdtkValue: Option[WDTKValue] = None
) extends Entity {

  val entityId: EntityId = propertyId

  def iri: IRI = IRI(siteIri + "/" + propertyId.id)

  override def toString = s"${propertyId.id}-${labels.get(WBLang("en")).getOrElse("")}@$vertexId"

  lazy val prec: PropertyRecord = PropertyRecord(propertyId, vertexId)

  override def withLocalStatement(
      prec: PropertyRecord,
      literal: LiteralValue,
      qs: List[Qualifier] = List()
  ): Property =
    this.copy(
      localStatements = this.localStatements :+ LocalStatement(prec, literal, qs)
    )

  override def withStatement(s: WDStatement): Entity = this

  override def withOkShapes(shapes: Set[ShapeLabel]): Entity =
    this.copy(okShapes = shapes)

  override def addPropertyValues(pid: PropertyId, values: List[WDTKValue]): Entity = {
    println(s"AddPropertyValues: $pid not implemented yet")
    this
  }

}
object Property {
  def fromPropertyDocument(pd: PropertyDocument): Property = {
    val propertyId = PropertyId(pd.getEntityId().getId(), IRI(pd.getEntityId().getIri()))
    Property(
      propertyId,
      VertexId(0L),
      Map(),
      Map(),
      Map(),
      pd.getEntityId().getSiteIri(),
      List(),
      Datatype(pd.getDatatype().getIri()),
      Set()
    )
  }
}
