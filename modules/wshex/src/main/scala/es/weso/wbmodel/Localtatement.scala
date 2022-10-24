package es.weso.wbmodel

case class LocalStatement(
    propertyRecord: PropertyRecord,
    literal: LiteralValue,
    qualifiers: List[Qualifier]
) {

  def withQualifiers(qs: List[Qualifier]): LocalStatement =
    this.copy(qualifiers = qs)

  override def toString = s"$propertyRecord - $literal${
      if (qualifiers.isEmpty) "" else s"{{" + qualifiers.map(_.toString).mkString(",") + "}}"
    }"
}

object LocalStatement {
  implicit val orderingById: Ordering[LocalStatement] = Ordering.by(_.propertyRecord.id)
}
