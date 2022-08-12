package es.weso.wshex
import es.weso.wbmodel._
import es.weso.rdf.nodes._

sealed trait ValueSetValue
sealed trait NonLocalValueSetValue extends ValueSetValue
sealed trait LocalValueSetValue extends ValueSetValue

case class EntityIdValueSetValue(id: EntityId) extends NonLocalValueSetValue
case class IRIValueSetValue(iri: IRI) extends LocalValueSetValue
case class StringValueSetValue(str: String) extends LocalValueSetValue
