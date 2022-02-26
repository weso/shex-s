package es.weso.shex.implicits

import es.weso.rdf.{Prefix, PrefixMap}
import es.weso.rdf.nodes.IRI
import es.weso.shex._
import munit._
import cats.implicits._

class EqShExTest extends FunSuite {
  test(s"Should compare single schemas without ignoring namespaces") {
    val s1            = Schema.empty
    val pm: PrefixMap = PrefixMap(Map(Prefix("ex") -> IRI("http://example.org/")))
    val s2            = Schema.empty.withPrefixMap(Some(pm))
    assertNotEquals(s1, s2)
  }

}
