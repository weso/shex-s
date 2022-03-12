package es.weso.shex

import es.weso.rdf.operations.Comparisons.NumericLiteral

sealed trait XsFacet {
  val fieldName: String
  def sameTypeAs(other: XsFacet): Boolean
}

sealed trait StringFacet extends XsFacet

case class Length(v: Int) extends StringFacet {
  val fieldName = "length"
  override def sameTypeAs(other: XsFacet): Boolean = other match {
    case _: Length => true
    case _         => false
  }
}

case class MinLength(v: Int) extends StringFacet {
  val fieldName = "minlength"
  override def sameTypeAs(other: XsFacet): Boolean = other match {
    case _: MinLength => true
    case _            => false
  }
}

case class MaxLength(v: Int) extends StringFacet {
  val fieldName = "maxlength"
  override def sameTypeAs(other: XsFacet): Boolean = other match {
    case _: MaxLength => true
    case _            => false
  }

}

case class Pattern(p: String, flags: Option[String]) extends StringFacet {
  val fieldName = "pattern"
  override def sameTypeAs(other: XsFacet): Boolean = other match {
    case _: Pattern => true
    case _          => false
  }

}

sealed trait NumericFacet extends XsFacet

case class MinInclusive(n: NumericLiteral) extends NumericFacet {
  val fieldName = "mininclusive"
  override def sameTypeAs(other: XsFacet): Boolean = other match {
    case _: MinInclusive => true
    case _               => false
  }
}
case class MinExclusive(n: NumericLiteral) extends NumericFacet {
  val fieldName = "minexclusive"
  override def sameTypeAs(other: XsFacet): Boolean = other match {
    case _: MinExclusive => true
    case _               => false
  }
}

case class MaxInclusive(n: NumericLiteral) extends NumericFacet {
  val fieldName = "maxinclusive"
  override def sameTypeAs(other: XsFacet): Boolean = other match {
    case _: MaxInclusive => true
    case _               => false
  }
}

case class MaxExclusive(n: NumericLiteral) extends NumericFacet {
  val fieldName = "maxexclusive"
  override def sameTypeAs(other: XsFacet): Boolean = other match {
    case _: MaxExclusive => true
    case _               => false
  }
}

case class TotalDigits(n: Int) extends NumericFacet {
  val fieldName = "totaldigits"
  override def sameTypeAs(other: XsFacet): Boolean = other match {
    case _: TotalDigits => true
    case _              => false
  }
}

case class FractionDigits(n: Int) extends NumericFacet {
  val fieldName = "fractiondigits"
  override def sameTypeAs(other: XsFacet): Boolean = other match {
    case _: FractionDigits => true
    case _                 => false
  }
}
