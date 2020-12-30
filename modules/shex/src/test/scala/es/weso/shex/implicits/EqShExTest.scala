package es.weso.shex.implicits

import es.weso.rdf.{Prefix, PrefixMap}
import es.weso.rdf.nodes.IRI
import es.weso.shex._
import org.scalatest._
import cats.implicits._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EqShExTest extends AnyFunSpec with Matchers with EitherValues {
  describe(s"Eq ShEx Schema") {
    it(s"Should compare single schemas without ignoring namespaces") {
      val s1 = Schema.empty 
      val pm: PrefixMap = PrefixMap(Map(Prefix("ex") -> IRI("http://example.org/")))
      val s2 = Schema.empty.copy(prefixes = Some(pm)) 
      s1 === s2 should be(false)
    }
  }

}
