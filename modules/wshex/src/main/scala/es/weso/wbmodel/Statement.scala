package es.weso.wbmodel

import org.wikidata.wdtk.datamodel.implementation._
import org.wikidata.wdtk.datamodel.interfaces.{Snak => WDTKSnak, Statement => WDTKStatement, _}
import cats.implicits._

case class Statement(
    propertyId: PropertyId,
    snak: Snak,
    qualifiers: Qualifiers,
    references: References
) {

  def withQualifiers(qs: Qualifiers): Statement =
    this.copy(qualifiers = qs)

  def withReferences(refs: References): Statement =
    this.copy(references = refs)

  override def toString = s"$propertyId - $snak"

}

object Statement {
  implicit val orderingById: Ordering[Statement] = Ordering.by(_.propertyId.id)

  def fromWDTKStatement(s: WDTKStatement): Statement = {
    val prop = PropertyId.fromPropertyIdValue(s.getMainSnak().getPropertyId())
    val snak = Snak.fromWDTKSnak(s.getMainSnak())
    val qualifiers = Qualifiers.fromSnakGroups(s.getQualifiers())
    val references = References.fromWDTKReferences(s.getReferences())
    Statement(prop, snak, qualifiers, references)
  }
}
