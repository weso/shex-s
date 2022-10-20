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

abstract trait Value extends Product with Serializable

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

object Value {

  lazy val siteDefault = "http://www.wikidata.org/entity/"
  lazy val defaultIRI = IRI(siteDefault)

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
      label.fold(Map[WBLang, String]())(lbl => Map(WBLang("en") -> lbl)),
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
      case iv: ItemIdValue     => ItemId(iv.getId(), IRI(iv.getIri()))
      case pv: PropertyIdValue => PropertyId(pv.getId(), IRI(pv.getIri()))
      case other               => NotImplementedWDTKValue(v, other.getEntityType())
    }
    override def visit(v: GlobeCoordinatesValue): Value = NotImplementedWDTKValue(v, "Quantity")
    override def visit(v: MonolingualTextValue): Value =
      NotImplementedWDTKValue(v, "MonolingualText")
    override def visit(v: WDQuantityValue): Value =
      QuantityValue(v.getNumericValue(), v.getLowerBound(), v.getUpperBound(), v.getUnitItemId())
    override def visit(v: WDStringValue): Value = StringValue(v.getString())
    override def visit(v: TimeValue): Value = NotImplementedWDTKValue(v, "Time")
    override def visit(v: UnsupportedValue): Value = NotImplementedWDTKValue(v, "Unsupported")
  }

}
