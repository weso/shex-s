package es.weso.wshex
import es.weso.wbmodel._
import es.weso.rbe.interval.IntOrUnbounded

sealed abstract class PropertySpec
object PropertySpec {
 case class EachOfPs(
  ps: List[PropertySpec],
  min: Int,
  max: IntOrUnbounded
  ) extends PropertySpec

 case class OneOfPs(
  ps: List[PropertySpec],
  min: Int,
  max: IntOrUnbounded
  ) extends PropertySpec
 case object EmptySpec extends PropertySpec
 
 sealed abstract class PropertyConstraint extends PropertySpec
 object PropertyConstraint {
  case class PropertyLocal(
    p: PropertyId,
    nc: WNodeConstraint,
    min: Int,
    max: IntOrUnbounded
  ) extends PropertyConstraint
  case class PropertyRef(
    p: PropertyId,
    ref: WShapeRef,
    min: Int,
    max: IntOrUnbounded
) extends PropertyConstraint
 }
}