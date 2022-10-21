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
  Value => WDTKValue,
  SiteLink => WDTKSiteLink,
  _
}
import org.wikidata.wdtk.datamodel.implementation._

abstract trait Value extends Product with Serializable {
  val wdtkValue: Option[WDTKValue]
  def toWDTKValue: WDTKValue
}

sealed abstract class LiteralValue extends Value

case class StringValue(
    str: String,
    wdtkValue: Option[WDTKValue] = None
) extends LiteralValue {
  override def toString = s"$str"
  override def toWDTKValue: WDTKValue = 
    wdtkValue.fold(StringValueImpl(str))(identity)

}

case class QuantityValue(
    numericValue: java.math.BigDecimal,
    lowerBound: java.math.BigDecimal,
    upperBound: java.math.BigDecimal,
    unit: ItemIdValue,
    wdtkValue: Option[WDTKValue] = None
) extends Value {

  override def toWDTKValue: WDTKValue = 
    wdtkValue.fold(QuantityValueImpl(numericValue,lowerBound,upperBound,unit))(identity)

}

case class IRIValue(
    iri: IRI,
    wdtkValue: Option[WDTKValue] = None
) extends LiteralValue {
  override def toString = s"${iri.getLexicalForm}"

  override def toWDTKValue: WDTKValue = 
    wdtkValue.fold(
      throw new RuntimeException(s"IRIValue.toWDTKValue: Converting IRIValue to WDTK...pending IRIValueImpl???"))(identity)

}

case class NotImplementedWDTKValue(wdtkValue: Option[WDTKValue], name: String) extends Value {
  override def toWDTKValue: WDTKValue = 
    wdtkValue.fold(
      throw new RuntimeException(s"NotImplementedValue.toWDTKValue: Converting NotImplementedValue to WDTK...pending NotImplementedValueImpl???"))(identity)
}

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

  def fromWDTKValue(v: WDTKValue): Value = {
    val convertVisitor = ConvertValueVisitor()
    v.accept(convertVisitor)
  }


  private case class ConvertValueVisitor() extends ValueVisitor[Value] {

    override def visit(v: EntityIdValue): Value = v match {
      case iv: ItemIdValue     => ItemId(iv.getId(), IRI(iv.getIri()))
      case pv: PropertyIdValue => PropertyId(pv.getId(), IRI(pv.getIri()))
      case other               => NotImplementedWDTKValue(v.some, other.getEntityType())
    }
    override def visit(v: GlobeCoordinatesValue): Value = 
      NotImplementedWDTKValue(v.some, "Quantity")
    override def visit(v: MonolingualTextValue): Value =
      NotImplementedWDTKValue(v.some, "MonolingualText")
    override def visit(v: WDQuantityValue): Value =
      QuantityValue(v.getNumericValue(), v.getLowerBound(), v.getUpperBound(), v.getUnitItemId())
    override def visit(v: WDStringValue): Value = StringValue(v.getString())
    override def visit(v: TimeValue): Value = 
      NotImplementedWDTKValue(v.some, "Time")
    override def visit(v: UnsupportedValue): Value = 
      NotImplementedWDTKValue(v.some, "Unsupported")
  }

}
