package es.weso.wshex
import es.weso.wbmodel._
import es.weso.rdf.nodes._

sealed trait NoMatchValueSetValue {
    val value: Value
    val vsvalue: ValueSetValue
}

case class NoMatchValueSetValue_NonLocal(value: Value, vsvalue: NonLocalValueSetValue) extends NoMatchValueSetValue
case class NoMatchValueSetValue_EntityIdDifferent(value: Value, vsvalue: LocalValueSetValue) extends NoMatchValueSetValue
case class NoMatchValueSetValue_NotImplemented(value: Value, vsvalue: LocalValueSetValue) extends NoMatchValueSetValue
case class NoMatchValueSetValue_IRIStem(value: Value, vsvalue: LocalValueSetValue) extends NoMatchValueSetValue
