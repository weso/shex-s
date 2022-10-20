package es.weso.wbmodel

sealed abstract class Qualifier extends Product with Serializable {
  val propertyId: PropertyId
  val snak: Snak
}

case class EntityQualifier(
    propertyId: PropertyId,
    entity: Entity
) extends Qualifier {
  override val snak: Snak = Snak.ValueSnak(entity)
  override def toString = s"$propertyId:$entity"
}

case class LocalQualifier(
    propertyId: PropertyId,
    literal: LiteralValue
) extends Qualifier {
  override val snak: Snak = Snak.ValueSnak(literal)
  override def toString = s"$propertyId:$literal"
}
