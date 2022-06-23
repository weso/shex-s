package es.weso.wshex
import es.weso.wbmodel._

case class QualifierSpec(ps: PropertySpec, closed: Boolean)

sealed abstract class PropertySpec
case class EachOfPs(ps: List[PropertySpec]) extends PropertySpec
case class OneOfPs(ps: List[PropertySpec]) extends PropertySpec
case object EmptySpec extends PropertySpec
case class Qualifier(p: PropertyId, ve: ShapeExpr) extends PropertySpec
