package es.weso

import es.weso.rdf.nodes._
import es.weso.rbe.interval.{IntLimit, IntOrUnbounded}

package object wshex {

  type Min = Int
  type Max = IntOrUnbounded

  lazy val defaultMin = 1
  lazy val defaultMax = IntLimit(1)

  val prov = IRI("http://www.w3.org/ns/prov#")
  val `prov:wasDerivedFrom` = prov + "wasDerivedFrom"

}
