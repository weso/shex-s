package es.weso.wbmodel

import org.wikidata.wdtk.datamodel.interfaces.{
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
import _root_.java.io.ByteArrayOutputStream
import es.weso.utils.internal.CollectionCompat._
import org.wikidata.wdtk.wikibaseapi.WikibaseDataFetcher
import org.wikidata.wdtk.datamodel.helpers.ItemDocumentBuilder
import org.wikidata.wdtk.datamodel.helpers.PropertyDocumentBuilder

case class EntityDoc(entityDocument: EntityDocument) extends Serializable {

  private val mapper = new ObjectMapper()
  mapper.configure(JsonGenerator.Feature.AUTO_CLOSE_TARGET, false)

  def getID(): String = entityDocument.getEntityId().getId()
  def getType(): String = entityDocument.getEntityId().getEntityType()

  lazy val valueMap: Map[PropertyIdValue, List[WDTKValue]] = entityDocument match {
    case s: StatementDocument =>
      val r = s
        .getStatementGroups()
        .asScala
        .toList
        .map(sg => (sg.getProperty(), sg.getStatements().asScala.toList.map(_.getValue())))
      r.toMap

    case _ => Map()
  }

  def getValues(property: PropertyIdValue): List[WDTKValue] =
    valueMap.get(property).getOrElse(List())

  def asJsonStr(): String =
    mapper.writeValueAsString(entityDocument)

  def showValue(value: WDTKValue, options: ShowEntityOptions): String = value match {
    case g: GlobeCoordinatesValue => g.toString
    case e: EntityIdValue         => e.getId()
    case i: IriIdentifiedValue    => i.getIri()
    case m: MonolingualTextValue  => m.getText()
    case q: QuantityValue         => q.getNumericValue().toString()
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

  /*  def merge(other: EntityDoc): EntityDoc =
    // TODO
    EntityDoc(this.entityDocument)

  def addProperty(pid: PropertyId): EntityDoc =
    this.copy(entityDo = entityDocument.) */
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

}
