package es.weso.wshex
import es.weso.wbmodel._
import es.weso.rbe.interval.IntOrUnbounded

sealed abstract class PropertySpec

case class EachOfPs(ps: List[PropertySpec]) extends PropertySpec

case class OneOfPs(ps: List[PropertySpec]) extends PropertySpec

case object EmptySpec extends PropertySpec

sealed abstract class PropertyS extends PropertySpec
case class PropertyLocal(
    p: PropertyId,
    nc: WNodeConstraint,
    min: Int,
    max: IntOrUnbounded
) extends PropertyS
case class PropertyRef(
    p: PropertyId,
    ref: WShapeRef,
    min: Int,
    max: IntOrUnbounded
) extends PropertyS
