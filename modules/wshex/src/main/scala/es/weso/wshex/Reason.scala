package es.weso.wshex

import es.weso.collection.Bag
import es.weso.rbe.Rbe
import cats.data._
import es.weso.rbe.RbeError
import es.weso.rbe.interval.IntOrUnbounded
import es.weso.wbmodel._

case class ReasonCode(code: Int) extends AnyVal

sealed abstract class Reason(val errCode: ReasonCode)            extends Product with Serializable
case class NoValueForProperty(prop: Property)                    extends Reason(errCode = Reason.noValueForProperty)
case class ValueIsNot(expectedId: String)                        extends Reason(errCode = Reason.valueIsNot)
case class ShapeNotFound(shapeLabel: ShapeLabel, schema: Schema) extends Reason(errCode = Reason.shapeNotFound)
case class NoMatch(
    bag: Bag[(PropertyId, ShapeLabel)],
    rbe: Rbe[(PropertyId, ShapeLabel)],
    errors: NonEmptyList[RbeError]
) extends Reason(errCode = Reason.noMatch)
case class NoValueValueSet(value: Value, valueSet: List[ValueSetValue]) extends Reason(errCode = Reason.noValueValueSet)
case class NoStringDatatype(value: Value)   extends Reason(errCode = Reason.noStringDatatype)
case class NoDateDatatype(value: Value)     extends Reason(errCode = Reason.noDateDatype)
case class ErrorsMatching(es: List[Reason]) extends Reason(errCode = Reason.errorsMatching)
case class CardinalityError(p: PropertyId, count: Int, min: Int, max: IntOrUnbounded)
    extends Reason(errCode = Reason.cardinalityError)
case class WaitingForFailed(es: Set[(Value, PropertyId, ShapeLabel)]) extends Reason(errCode = Reason.waitingForFailed)
case class MatchNot(bag: Bag[(PropertyId, ShapeLabel)], rbe: Rbe[(PropertyId, ShapeLabel)])
    extends Reason(errCode = Reason.matchNot)
case class ShapeOr_AllFailed(es: List[Reason]) extends Reason(errCode = Reason.shapeOr_AllFailed)
case class NotImplemented(msg: String)         extends Reason(errCode = Reason.notImplemented)
case class NotAllowedNotInExtra(notAllowed: List[(PropertyId, Int)])
    extends Reason(errCode = Reason.notAllowedNotInExtra)
case class FailedPropsNotExtra(ps: Set[(PropertyId, ShapeLabel)]) extends Reason(errCode = Reason.failedPropsNotExtra)
case class NullEntity(fromLabel: ShapeLabel)                      extends Reason(errCode = Reason.nullEntity)
case class NoneMatchShapeOr(entity: Entity, so: ShapeOr)          extends Reason(errCode = Reason.noneMatchShapeOr)

object Reason {
  val noValueForProperty   = ReasonCode(0)
  val valueIsNot           = ReasonCode(1)
  val shapeNotFound        = ReasonCode(2)
  val noMatch              = ReasonCode(3)
  val noValueValueSet      = ReasonCode(4)
  val noStringDatatype     = ReasonCode(5)
  val noDateDatype         = ReasonCode(6)
  val errorsMatching       = ReasonCode(7)
  val cardinalityError     = ReasonCode(8)
  val waitingForFailed     = ReasonCode(9)
  val matchNot             = ReasonCode(10)
  val shapeOr_AllFailed    = ReasonCode(11)
  val notImplemented       = ReasonCode(12)
  val notAllowedNotInExtra = ReasonCode(13)
  val failedPropsNotExtra  = ReasonCode(14)
  val nullEntity           = ReasonCode(15)
  val noneMatchShapeOr     = ReasonCode(16)
}
