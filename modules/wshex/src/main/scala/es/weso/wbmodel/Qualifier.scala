package es.weso.wbmodel

sealed abstract class Qualifier extends Product with Serializable {
  val propertyId: PropertyId
  val snak: Snak
}

case class EntityIdQualifier(
    propertyId: PropertyId,
    entityId: EntityId
) extends Qualifier {
  override val snak: Snak = Snak.ValueSnak(entityId, propertyId)
  override def toString = s"$propertyId:$entityId"
}

case class LocalQualifier(
    propertyId: PropertyId,
    literal: LiteralValue
) extends Qualifier {
  override val snak: Snak = Snak.ValueSnak(literal, propertyId)
  override def toString = s"$propertyId:$literal"
}
