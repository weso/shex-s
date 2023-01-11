package es.weso.wbmodel

import org.wikidata.wdtk.datamodel.interfaces.{
  QuantityValue => WDTKQuantityValue,
  Snak => WDTKSnak,
  Statement => WDTKStatement,
  StringValue => WDTKStringValue,
  Value => WDTKValue,
  _
}
import org.wikidata.wdtk.datamodel.helpers.JsonDeserializer
import org.wikidata.wdtk.datamodel.helpers.JsonSerializer
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.exc._
import cats.effect._
import collection.JavaConverters._
import es.weso.utils.internal.CollectionCompat._
import org.wikidata.wdtk.wikibaseapi.WikibaseDataFetcher
import org.wikidata.wdtk.datamodel.helpers.ItemDocumentBuilder
import org.wikidata.wdtk.datamodel.helpers.PropertyDocumentBuilder
import org.wikidata.wdtk.datamodel.implementation.ItemIdValueImpl
import org.wikidata.wdtk.datamodel.helpers.StatementBuilder
import org.wikidata.wdtk.datamodel.helpers.Datamodel
import EntityDocError._
import es.weso.rdf.nodes._
import es.weso.utils.internal.CollectionCompat._

/** EntityDoc is a Scala wrapper for WDTK EntityDocuments
  */
case class EntityDoc(entityDocument: EntityDocument) extends Serializable {

  private val mapper = new ObjectMapper()
  mapper.configure(JsonGenerator.Feature.AUTO_CLOSE_TARGET, false)

  def getID(): String = entityDocument.getEntityId().getId()
  def getType(): String = entityDocument.getEntityId().getEntityType()

  lazy val valueMap: Map[PropertyIdValue, LazyList[WDTKValue]] = entityDocument match {
    case s: StatementDocument =>
      val r = s
        .getStatementGroups()
        .asScala
        .toList
        .map(sg => (sg.getProperty(), sg.getStatements().asScala.toLazyList.map(_.getValue())))
      r.toMap

    case _ => Map()
  }

  def getStatements(): List[WDTKStatement] = entityDocument match {
    case s: StatementDocument =>
      s.getAllStatements().asScala.toList
    case _ => List()
  }

  def getStatementsForProperty(
      prop: PropertyIdValue
  ): List[WDTKStatement] = entityDocument match {
    case s: StatementDocument =>
      try
        s.findStatementGroup(prop).getStatements().asScala.toList
      catch {
        case e: Exception => List()
      }
    case _ => List()
  }

  def getLabels(): Map[String, MonolingualTextValue] =
    entityDocument match {
      case id: ItemDocument     => id.getLabels().asScala.toMap
      case pd: PropertyDocument => pd.getLabels().asScala.toMap
    }

  def getDescriptions(): Map[String, MonolingualTextValue] =
    entityDocument match {
      case id: ItemDocument     => id.getDescriptions().asScala.toMap
      case pd: PropertyDocument => pd.getDescriptions().asScala.toMap
    }

  def getAliases(): Map[String, List[MonolingualTextValue]] =
    entityDocument match {
      case id: ItemDocument     => id.getAliases().asScala.mapValues(_.asScala.toList).toMap
      case pd: PropertyDocument => pd.getAliases().asScala.mapValues(_.asScala.toList).toMap
    }

  def getValues(property: PropertyIdValue): LazyList[WDTKValue] =
    valueMap.get(property).getOrElse(LazyList())

  def asJsonStr(): String =
    mapper.writeValueAsString(entityDocument)

  def showValue(value: WDTKValue, options: ShowEntityOptions): String = value match {
    case g: GlobeCoordinatesValue => g.toString
    case e: EntityIdValue         => e.getId()
    case i: IriIdentifiedValue    => i.getIri()
    case m: MonolingualTextValue  => m.getText()
    case q: WDTKQuantityValue     => q.getNumericValue().toString()
    case s: WDTKStringValue       => s.getString()
    case t: TimeValue             => t.toString()
    case u: UnsupportedValue      => u.toString()
    case o =>
      if (options.showAllValues)
        if (o == null) "no value"
        else o.toString
      else
        "?"
  }

  def showStatement(s: WDTKStatement, options: ShowEntityOptions): String =
    s"${s.getMainSnak().getPropertyId().getId()}/${showValue(s.getValue(), options)}"

  private def getNumber(s: String): Int = {
    val p = "P(\\d+)".r
    p.findFirstMatchIn(s) match {
      case Some(nn) => nn.group(1).toInt
      case _        => 0
    }
  }

  private def compareProperty(s1: WDTKStatement, s2: WDTKStatement): Boolean =
    getNumber(s1.getMainSnak().getPropertyId().getId()) < getNumber(
      s2.getMainSnak().getPropertyId().getId()
    )

  def showStatements(options: ShowEntityOptions): String =
    entityDocument match {
      case s: StatementDocument =>
        val ss = s.getAllStatements().asScala.toList.sortWith(compareProperty)
        val ps = options.maxStatements match {
          case Some(m) => ss.take(m)
          case None    => ss
        }
        ps.map(showStatement(_, options)).mkString(",")
      case _ => "{}"
    }

  def show(options: ShowEntityOptions = ShowEntityOptions.default): String =
    s"${entityDocument.getEntityId().getId()} ${showStatements(options)}"

  def merge(other: EntityDoc): EntityDoc =
    mergeStatements(other.getStatements())

  def addPropertyValues(
      pidValue: PropertyIdValue,
      values: LazyList[WDTKValue]
  ): EntityDoc = {
    val sb: StatementBuilder =
      StatementBuilder.forSubjectAndProperty(entityDocument.getEntityId(), pidValue)
    val st: WDTKStatement = values
      .foldLeft(sb) { case (c, value) =>
        c.withValue(value)
      }
      .build()
    EntityDoc(entityDocument match {
      case id: ItemDocument     => id.withStatement(st)
      case pd: PropertyDocument => pd.withStatement(st)
    })
  }

  def addStatement(st: Statement): EntityDoc = {
    val property = st.propertyId
    val sb: StatementBuilder =
      StatementBuilder
        .forSubjectAndProperty(entityDocument.getEntityId(), property.toWDTKValue)

    val sbSnak = st.snak match {
      case _: Snak.NoValueSnak   => sb.withNoValue()
      case _: Snak.SomeValueSnak => sb.withSomeValue()
      case vs: Snak.ValueSnak    => sb.withValue(vs.value.toWDTKValue)
    }
    val sbQs = sbSnak.withQualifiers(st.qualifiers.snakGroups.asJava)
    val sbRefs = sbQs.withReferences(st.references.asWDTKReferences.asJava)
    val wdStatement = sbRefs.build()

    EntityDoc(entityDocument match {
      case id: ItemDocument     => id.withStatement(wdStatement)
      case pd: PropertyDocument => pd.withStatement(wdStatement)
    })
  }

  def withLabel(langCode: String, label: String): EntityDoc =
    entityDocument match {
      case td: TermedDocument =>
        EntityDoc(td.withLabel(Datamodel.makeMonolingualTextValue(label, langCode)))
      case _ => throw NotTermedDocument(entityDocument)
    }

  def withDescription(langCode: String, descr: String): EntityDoc =
    entityDocument match {
      case td: TermedDocument =>
        EntityDoc(td.withDescription(Datamodel.makeMonolingualTextValue(descr, langCode)))
      case _ => throw NotTermedDocument(entityDocument)

    }
  def withAliases(langCode: String, aliases: List[String]): EntityDoc =
    entityDocument match {
      case td: TermedDocument =>
        val cnvAliases = aliases.map(Datamodel.makeMonolingualTextValue(_, langCode)).asJava
        EntityDoc(td.withAliases(langCode, cnvAliases))
      case _ => throw NotTermedDocument(entityDocument)
    }

  def mergeStatements(ss: List[WDTKStatement]): EntityDoc = {
    val ed = ss.foldLeft(entityDocument) { case (c, s) =>
      c match {
        case id: ItemDocument     => id.withStatement(s)
        case pd: PropertyDocument => pd.withStatement(s)
      }
    }
    EntityDoc(ed)
  }

}

object EntityDoc {

  /** Get Entity from a JSON string
    *
    * The JSON format is the same as the one used by dumps
    *
    * @param str
    * @param jsonDeserializer
    * @return an action that once run will contain the Entity that represents the JSON entity
    */
  def fromJsonStr(
      str: String,
      jsonDeserializer: JsonDeserializer
  ): IO[EntityDoc] = IO {
    EntityDoc(jsonDeserializer.deserializeEntityDocument(str))
  }

//  def fromEntityDocument(ed: EntityDocument): EntityDoc = EntityDoc(ed)

  private lazy val wbdf: WikibaseDataFetcher = WikibaseDataFetcher.getWikidataDataFetcher()

  /** Get an entity from Wikidata
    *
    * @param entity
    * @return entity
    */
  def fetchEntity(entity: String): IO[EntityDoc] = for {
    entityDocument <- IO(wbdf.getEntityDocument(entity))
  } yield EntityDoc(entityDocument)

  def emptyFrom(e: EntityDocument): EntityDoc = {
    val ed = e match {
      case id: ItemDocument =>
        ItemDocumentBuilder.forItemId(id.getEntityId()).build()
      case pd: PropertyDocument =>
        PropertyDocumentBuilder.forPropertyIdAndDatatype(pd.getEntityId(), pd.getDatatype()).build()
      case _ =>
        throw new Exception(s"emptyFromEntityDoc: Unsupported type of entity document yet: $e")
    }
    EntityDoc(ed)
  }

  lazy val defaultSite = "http://www.wikidata.org/entity/"

  def QId(num: Long, site: String = defaultSite): EntityDoc = {
    val id: ItemIdValue = new ItemIdValueImpl(s"Q$num", site)
    EntityDoc(ItemDocumentBuilder.forItemId(id).build())
  }

}
