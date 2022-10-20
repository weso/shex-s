package es.weso.wbmodel

import org.wikidata.wdtk.datamodel.implementation._
import org.wikidata.wdtk.datamodel.interfaces._
import cats.implicits._

case class Statement(
    propertyId: PropertyId,
    snak: Snak,
    qualifiers: Option[Qualifiers],
    references: Option[List[Reference]]
) {

  def withQualifiers(qs: Qualifiers): Statement =
    this.copy(qualifiers = qs.some)

  override def toString = s"$propertyId - $snak"
}

object Statement {
  implicit val orderingById: Ordering[Statement] = Ordering.by(_.propertyId.id)
}
