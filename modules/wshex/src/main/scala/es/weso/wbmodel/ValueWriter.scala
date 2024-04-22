package es.weso.wbmodel

import es.weso.wshex.ShapeLabel
import org.wikidata.wdtk.datamodel.helpers.JsonSerializer
import org.wikidata.wdtk.datamodel.implementation._
import org.wikidata.wdtk.datamodel.interfaces.{SiteLink => WBSiteLink, _}
import java.io.ByteArrayOutputStream
import scala.collection.JavaConverters._
import es.weso.rdf.RDFBuilder

object ValueWriter {

  def entity2JsonStr(v: Entity, showShapes: Boolean): String = {
    val os = new ByteArrayOutputStream()
    val str = entity2entityDocument(v) match {
      case (id: ItemDocument, okShapes) =>
        JsonSerializer.getJsonString(id) ++ printShapes(showShapes, okShapes)
      case (pd: PropertyDocument, okShapes) =>
        JsonSerializer.getJsonString(pd) ++ printShapes(showShapes, okShapes)
      case _ => ""
    }
    str
  }

  private def printShapes(showShapes: Boolean, shapes: Set[ShapeLabel]): String =
    if (showShapes && shapes.nonEmpty) {
      "// Shapes: " ++ shapes.map(_.name).mkString(",")
    } else ""

  def entity2entityDocument(v: Entity): (EntityDocument, Set[ShapeLabel]) = v match {
    case i: Item =>
      val ed = new ItemDocumentImpl(
        cnvItemId(i.itemId),
        cnvMultilingual(i.labels).asJava,
        cnvMultilingual(i.descriptions).asJava,
        cnvMultilingual(i.aliases).asJava,
        cnvStatements(i.localStatements).asJava,
        cnvSiteLinks(i.siteLinks).asJava,
        0L
      )
      (ed, i.okShapes)
    case p: Property =>
      val pd = new PropertyDocumentImpl(
        cnvPropertyId(p.propertyId),
        cnvMultilingual(p.labels).asJava,
        cnvMultilingual(p.descriptions).asJava,
        cnvMultilingual(p.aliases).asJava,
        cnvStatements(p.localStatements).asJava,
        cnvDatatype(p.datatype),
        0L
      )
      (pd, p.okShapes)
  }

  def cnvMultilingual(m: Map[WBLang, String]): List[MonolingualTextValue] =
    m.toList.map { case (lang, text) =>
      new MonolingualTextValueImpl(text, lang.code)
    }

  def cnvItemId(id: ItemId): ItemIdValue =
    new ItemIdValueImpl(id.id, id.iri.getLexicalForm)

  def cnvPropertyId(pd: PropertyId): PropertyIdValue =
    new PropertyIdValueImpl(pd.id, pd.iri.getLexicalForm)

  def cnvStatements(ls: List[LocalStatement]): List[StatementGroup] = List()

  def cnvSiteLinks(sl: List[SiteLink]): List[WBSiteLink] = List()

  def cnvDatatype(dt: Datatype): DatatypeIdValue = new DatatypeIdImpl(dt.name)

}
