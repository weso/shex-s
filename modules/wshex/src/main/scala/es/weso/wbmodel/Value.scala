package es.weso.wbmodel

import cats.implicits._
import cats._
import es.weso.rdf.nodes._
import es.weso.wshex.ShapeLabel
import org.wikidata.wdtk.datamodel.interfaces.DatatypeIdValue

case class VertexId(value: Long) extends AnyVal

sealed abstract trait Value extends Product with Serializable

sealed abstract class EntityId extends Value {
  def id: String
  def iri: IRI
}
object EntityId {
  def fromIri(iri: IRI): EntityId = {
    val (name,base) = Utils.splitIri(iri)
    name(0) match {
      case 'P' => PropertyId(name,iri)
      case 'Q' => ItemId(name, iri)
      case _ =>
        throw new RuntimeException(s"""|Match error. EntityId.fromIri($iri):
                                       | localName: $name
                                       | base: $base
                                       | Should start by P or Q
                                       |""".stripMargin)
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
}

case class PropertyRecord(id: PropertyId, vertexId: VertexId) {
  override def toString=s"$id-$vertexId"
}

sealed abstract class Entity extends Value {
  val vertexId: VertexId
  val entityId: EntityId
  val localStatements: List[LocalStatement]
  // val okShapes: Set[ShapeLabel]

  def withLocalStatement(prec: PropertyRecord, literal: LiteralValue, qs: List[Qualifier]): Entity
  def localStatementsByPropId(propId: PropertyId) = {
    localStatements.filter(_.propertyRecord.id == propId)
  }
  def withOkShapes(shapes: Set[ShapeLabel]): Entity

}

case class ItemId(id: String, iri: IRI) extends EntityId {
  override def toString =s"$id"
}

case class Lang(code: String) extends AnyVal

case class Item(
                 itemId: ItemId,
                 vertexId: VertexId,
                 labels: Map[Lang,String],
                 descriptions: Map[Lang,String],
                 aliases: Map[Lang,String],
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
                                   qs: List[Qualifier] = List()): Item =
    this.copy(
      localStatements = this.localStatements :+ LocalStatement(prec,literal,qs)
    )

  override def withOkShapes(shapes: Set[ShapeLabel]): Entity = this.copy(okShapes = shapes)

}

case class Property(
                     propertyId: PropertyId,
                     vertexId: VertexId,
                     labels: Map[Lang,String],
                     descriptions: Map[Lang,String],
                     aliases: Map[Lang,String],
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
                                   qs: List[Qualifier] = List()): Property =
    this.copy(
      localStatements = this.localStatements :+ LocalStatement(prec,literal,qs)
    )

  override def withOkShapes(shapes: Set[ShapeLabel]): Entity =
    this.copy(okShapes = shapes)

}

sealed abstract class LiteralValue extends Value

case class StringValue(
                        str: String
                      ) extends LiteralValue {
  override def toString = s"$str"
}

case class DateValue(
                      date: String,
                    ) extends LiteralValue {
  override def toString = s"$date"
}

case class IRIValue(
                     iri: IRI,
                   ) extends LiteralValue {
  override def toString = s"${iri.getLexicalForm}"
}

sealed abstract class Qualifier
  extends Product with Serializable {
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
                      qualifiers: List[Qualifier] = List(),
                    ) {

  def id: PropertyId = propertyRecord.id

  def withQualifiers(qs: List[Qualifier]): Statement =
    this.copy(qualifiers = qs)

  override def toString = s"$propertyRecord ${if (qualifiers.isEmpty) "" else s"{{" + qualifiers.map(_.toString).mkString(",") + "}}" }"
}

case class LocalStatement(
                           propertyRecord: PropertyRecord,
                           literal: LiteralValue,
                           qualifiers: List[Qualifier]
                         ) {

  def withQualifiers(qs: List[Qualifier]): LocalStatement =
    this.copy(qualifiers = qs)

  override def toString = s"$propertyRecord - $literal${if (qualifiers.isEmpty) "" else s"{{" + qualifiers.map(_.toString).mkString(",") + "}}" }"
}

object LocalStatement {
  implicit val orderingById: Ordering[Statement] = Ordering.by(_.propertyRecord.id)
}

case class SiteLink(
                     title: String,
                     siteKey: String,
                     badges: List[ItemId]
                   )

case class Datatype(name: String) extends AnyVal
object Datatype {
  lazy val defaultDatatype = Datatype(DatatypeIdValue.DT_ITEM)
}

object Value {

  lazy val siteDefault = "http://www.wikidata.org/entity"


  def triple(
              subj: Entity, prop: Property, value: Entity
            ): (Entity, PropertyRecord, Entity, List[Qualifier]) = {
    (subj, prop.prec, value, List())
  }


  /*  def triple(
      subj: Entity, prop: PropertyRecord, value: Entity
      ): (Entity, PropertyRecord, Entity, List[Qualifier]) = {
      (subj, prop, value, List())
    } */

  def tripleq(
               subj: Entity,
               prop: Property,
               value: Entity,
               qs: List[Qualifier]
             ): (Entity, PropertyRecord, Entity, List[Qualifier]) = {
    (subj, prop.prec, value, qs)
  }

   def mkSite(base: String, localName: String) = IRI(base + "/" + localName)

 
  def Date(date: String): DateValue =
    DateValue(date)

  def Str(str: String): StringValue =
    StringValue(str)

  def Pid(num: Int, site: String = siteDefault): PropertyId = {
    val pid = "P" + num
    PropertyId(pid, mkSite(site, pid))
  }

  def Qid(num: Int, label: String, id: Long, site: String = Value.siteDefault): Item = {
    val qid = "Q" + num
    Item(ItemId(qid, iri = mkSite(site, qid)), VertexId(id), Map(Lang("en") -> label), Map(), Map(), site, List(), List())
  }

 
}