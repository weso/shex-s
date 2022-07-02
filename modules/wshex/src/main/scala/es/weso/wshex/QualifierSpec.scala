package es.weso.wshex
import es.weso.wbmodel._
import es.weso.rbe.interval.IntOrUnbounded

case class QualifierSpec(ps: PropertySpec, closed: Boolean)

sealed abstract class PropertySpec
case class EachOfPs(ps: List[PropertySpec]) extends PropertySpec
case class OneOfPs(ps: List[PropertySpec]) extends PropertySpec
case object EmptySpec extends PropertySpec
sealed abstract class QualifierS extends PropertySpec
case class QualifierLocal(
    p: PropertyId,
    nc: WNodeConstraint,
    min: Int,
    max: IntOrUnbounded
) extends QualifierS
case class QualifierRef(
    p: PropertyId,
    ref: WShapeRef,
    min: Int,
    max: IntOrUnbounded
) extends QualifierS
