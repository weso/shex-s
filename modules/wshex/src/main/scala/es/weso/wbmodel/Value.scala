package es.weso.wbmodel

import cats.implicits._
import cats._
import es.weso.rdf.nodes._
import es.weso.wshex.ShapeLabel
import org.wikidata.wdtk.datamodel.interfaces.{
  DatatypeIdValue,
  Statement => WDStatement,
  StringValue => WDStringValue,
  Value => WDValue,
  QuantityValue => WDQuantityValue,
  _
}
import org.wikidata.wdtk.datamodel.interfaces.EntityDocument
import org.wikidata.wdtk.datamodel.interfaces.ItemDocument
import org.wikidata.wdtk.datamodel.interfaces.PropertyDocument

sealed abstract trait Value extends Product with Serializable

sealed abstract class EntityId extends Value {
  def id: String
  def iri: IRI
}

object EntityId {

  def fromIri(iri: IRI): Either[String, EntityId] = {
    val (name, base) = Utils.splitIri(iri)
    name(0) match {
      case 'P' => PropertyId(name, iri).asRight
      case 'Q' => ItemId(name, iri).asRight
      case _ =>
        s"""|Match error. EntityId.fromIri($iri):
            | localName: $name
            | base: $base
            | Should start by P or Q
            |""".stripMargin.asLeft
    }
  }
}

case class PropertyId(
    id: String,
    iri: IRI
) extends EntityId {
  override def toString = s"$id"
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

case class PropertyRecord(id: PropertyId, vertexId: VertexId) {
  override def toString = s"$id-$vertexId"
}

sealed abstract class Entity extends Value {
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

  def mergeStatements(ls: List[WDStatement]): Entity = {
    ls.foldLeft(this) { 
      case (current, st) => current.withStatement(st) 
    }
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

case class ItemId(id: String, iri: IRI) extends EntityId {
  override def toString = s"$id"
}

case class Lang(code: String) extends AnyVal

case class Item(
    itemId: ItemId,
    vertexId: VertexId,
    labels: Map[Lang, String],
    descriptions: Map[Lang, String],
    aliases: Map[Lang, String],
    siteIri: String = Value.siteDefault,
    localStatements: List[LocalStatement],
    siteLinks: List[SiteLink],
    okShapes: Set[ShapeLabel] = Set()
) extends Entity {

  val entityId: EntityId = itemId
  def iri: IRI = IRI(siteIri + "/" + itemId.id)

  override def toString = s"${itemId.id}-${labels.get(Lang("en")).getOrElse("")}@$vertexId"

  override def withLocalStatement(
      prec: PropertyRecord,
      literal: LiteralValue,
      qs: List[Qualifier] = List()
  ): Item =
    this.copy(
      localStatements = this.localStatements :+ LocalStatement(prec, literal, qs)
    )

  override def withOkShapes(shapes: Set[ShapeLabel]): Entity = this.copy(okShapes = shapes)

  override def addPropertyValues(pid: PropertyId, value: List[WDValue]): Entity = {
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

case class Property(
    propertyId: PropertyId,
    vertexId: VertexId,
    labels: Map[Lang, String],
    descriptions: Map[Lang, String],
    aliases: Map[Lang, String],
    siteIri: String = Value.siteDefault,
    localStatements: List[LocalStatement] = List(),
    datatype: Datatype = Datatype.defaultDatatype,
    okShapes: Set[ShapeLabel] = Set()
) extends Entity {

  val entityId: EntityId = propertyId

  def iri: IRI = IRI(siteIri + "/" + propertyId.id)

  override def toString = s"${propertyId.id}-${labels.get(Lang("en")).getOrElse("")}@$vertexId"

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

  override def addPropertyValues(pid: PropertyId, values: List[WDValue]): Entity = {
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

sealed abstract class LiteralValue extends Value

case class StringValue(
    str: String
) extends LiteralValue {
  override def toString = s"$str"
}

case class DateValue(
    date: String
) extends LiteralValue {
  override def toString = s"$date"
}

case class QuantityValue(
  numericValue: java.math.BigDecimal,
  lowerBound: java.math.BigDecimal,
  upperBould: java.math.BigDecimal,
  unit: ItemIdValue
) extends Value


case class IRIValue(
    iri: IRI
) extends LiteralValue {
  override def toString = s"${iri.getLexicalForm}"
}

case class NotImplementedWDTKValue(v: WDValue, name: String) extends Value

sealed abstract class Qualifier extends Product with Serializable {
  val propertyId: PropertyId
  val value: Value
}

case class EntityQualifier(
    propertyId: PropertyId,
    entity: Entity
) extends Qualifier {
  override val value: Value = entity
  override def toString = s"$propertyId:$value"
}

case class LocalQualifier(
    propertyId: PropertyId,
    literal: LiteralValue
) extends Qualifier {
  override val value: Value = literal
  override def toString = s"$propertyId:$value"
}

case class Statement(
    propertyRecord: PropertyRecord,
    qualifiers: List[Qualifier] = List()
) {

  def id: PropertyId = propertyRecord.id

  def withQualifiers(qs: List[Qualifier]): Statement =
    this.copy(qualifiers = qs)

  override def toString = s"$propertyRecord ${
      if (qualifiers.isEmpty) "" else s"{{" + qualifiers.map(_.toString).mkString(",") + "}}"
    }"
}

case class LocalStatement(
    propertyRecord: PropertyRecord,
    literal: LiteralValue,
    qualifiers: List[Qualifier]
) {

  def withQualifiers(qs: List[Qualifier]): LocalStatement =
    this.copy(qualifiers = qs)

  override def toString = s"$propertyRecord - $literal${
      if (qualifiers.isEmpty) "" else s"{{" + qualifiers.map(_.toString).mkString(",") + "}}"
    }"
}

object LocalStatement {
  implicit val orderingById: Ordering[Statement] = Ordering.by(_.propertyRecord.id)
}

case class SiteLink(
    title: String,
    siteKey: String,
    badges: List[ItemId]
)

object Value {

  lazy val siteDefault = "http://www.wikidata.org/entity/"
  lazy val defaultIRI  = IRI(siteDefault)

  def triple(
      subj: Entity,
      prop: Property,
      value: Entity
  ): (Entity, PropertyRecord, Entity, List[Qualifier]) =
    (subj, prop.prec, value, List())

  def tripleq(
      subj: Entity,
      prop: Property,
      value: Entity,
      qs: List[Qualifier]
  ): (Entity, PropertyRecord, Entity, List[Qualifier]) =
    (subj, prop.prec, value, qs)

  def mkSite(base: String, localName: String) = IRI(base + localName)

  def Date(date: String): DateValue =
    DateValue(date)

  def Str(str: String): StringValue =
    StringValue(str)

  def Pid(num: Int, site: String = siteDefault): PropertyId = {
    val pid = "P" + num
    PropertyId(pid, mkSite(site, pid))
  }

  def Qid(
      num: Int,
      label: Option[String] = None,
      id: Long = 0L,
      site: String = Value.siteDefault
  ): Item = {
    val qid = "Q" + num
    Item(
      ItemId(qid, iri = mkSite(site, qid)),
      VertexId(id),
      label.fold(Map[Lang,String]())(lbl => Map(Lang("en") -> lbl)),
      Map(),
      Map(),
      site,
      List(),
      List()
    )
  }

  def fromWDTKValue(v: WDValue): Value = {
    val convertVisitor = ConvertValueVisitor()
    v.accept(convertVisitor)
  }

  private case class ConvertValueVisitor() extends ValueVisitor[Value] {

    override def visit(v: EntityIdValue): Value = v match {
      case iv: ItemIdValue => ItemId(iv.getId(), IRI(iv.getIri()))
      case pv: PropertyIdValue => PropertyId(pv.getId(), IRI(pv.getIri()))
      case other => NotImplementedWDTKValue(v, other.getEntityType())
    }
    override def visit(v: GlobeCoordinatesValue): Value = NotImplementedWDTKValue(v, "Quantity")
    override def visit(v: MonolingualTextValue): Value = NotImplementedWDTKValue(v, "MonolingualText")
    override def visit(v: WDQuantityValue): Value = 
      QuantityValue(v.getNumericValue(), v.getLowerBound(), v.getUpperBound(), v.getUnitItemId()
      )
    override def visit(v: WDStringValue): Value = StringValue(v.getString())
    override def visit(v: TimeValue): Value = NotImplementedWDTKValue(v, "Time")
    override def visit(v: UnsupportedValue): Value = NotImplementedWDTKValue(v, "Unsupported")
  }


}
