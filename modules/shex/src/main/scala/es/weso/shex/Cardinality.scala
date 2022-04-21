package es.weso.shex
import cats._
case class Cardinality(min: Int, max: Max) {

  def contains(n: Int): Boolean =
    n >= min && max.biggerThanOrEqual(n)

  def isDefault: Boolean = Cardinality.isDefault(min, max)

}

object Cardinality {
  lazy val defaultMin = 1
  lazy val defaultMax = IntMax(1)

  def isDefault(min: Int, max: Max): Boolean =
    min == defaultMin && max == defaultMax

  implicit lazy val showCardinality: Show[Cardinality] = new Show[Cardinality] {
    final def show(c: Cardinality): String = (c.min, c.max) match {
      case (0, Star)      => "*"
      case (0, IntMax(1)) => "?"
      case (1, Star)      => "+"
      case (m, Star)      => s"{$m,*}"
      case (m, IntMax(n)) => s"{$m,$n}"
    }
  }

}
