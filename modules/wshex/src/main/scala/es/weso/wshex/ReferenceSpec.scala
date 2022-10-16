package es.weso.wshex
import es.weso.wbmodel._
import es.weso.rbe.interval.IntOrUnbounded

sealed trait ReferencesSpec 
object ReferencesSpec {
case class ReferencesSpecSingle(
    ps: PropertySpec, 
    min: Int,
    max: IntOrUnbounded,
    closed: Boolean) extends ReferencesSpec
case class ReferencesOneOf(ls: List[ReferencesSpec]) extends ReferencesSpec
}

