package es.weso.wbmodel
import cats.implicits._
import cats._
import es.weso.rdf.nodes._
import es.weso.wshex.ShapeLabel
import org.wikidata.wdtk.datamodel.interfaces.{
  DatatypeIdValue,
  QuantityValue => WDQuantityValue,
  Statement => WDStatement,
  StringValue => WDStringValue,
  Value => WDValue,
  SiteLink => WDTKSiteLink,
  _
}
import org.wikidata.wdtk.datamodel.interfaces.EntityDocument
import org.wikidata.wdtk.datamodel.interfaces.ItemDocument
import org.wikidata.wdtk.datamodel.interfaces.PropertyDocument

abstract class Entity {
  val vertexId: VertexId
  val entityId: EntityId
  val localStatements: List[LocalStatement]
  // val okShapes: Set[ShapeLabel]

  def withLocalStatement(prec: PropertyRecord, literal: LiteralValue, qs: List[Qualifier]): Entity
  def addPropertyValues(pid: PropertyId, value: List[WDValue]): Entity
  def localStatementsByPropId(propId: PropertyId) =
    localStatements.filter(_.propertyRecord.id == propId)
  def withOkShapes(shapes: Set[ShapeLabel]): Entity

  def withStatement(s: WDStatement): Entity

  def mergeStatements(ls: List[WDStatement]): Entity =
    ls.foldLeft(this) { case (current, st) =>
      current.withStatement(st)
    }

  def merge(other: Entity): Entity = {
    println(s"merge for entity: $entityId not implemented yet")
    this
  }
}

object Entity {

  def fromEntityDocument(ed: EntityDocument): Entity =
    ed match {
      case id: ItemDocument =>
        Item.fromItemDocument(id)
      case pd: PropertyDocument =>
        Property.fromPropertyDocument(pd)
    }

}
