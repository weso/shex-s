package es.weso.shex
import munit._
import es.weso.rdf.nodes._
import es.weso.rdf._

class ShexDiffTest extends FunSuite {

  test("should calculate diffs of base values") {
    val iri = IRI("http://example.org/")
    val x1 = Schema.empty.withBase(Some(iri))
    val x2 = Schema.empty.withBase(Some(iri))
    val d = ShExDiff.schemaDiff(x1, x2)
    shouldBeOK(d)
  }
  test("should calculate diffs of start actions") {
    val iri = IRI("http://example.org/")
    val x1 = Schema.empty.withStartActions(Some(List(SemAct(iri, Some("code")))))
    val x2 = Schema.empty.withStartActions(Some(List(SemAct(iri, Some("code")))))
    val d = ShExDiff.schemaDiff(x1, x2)
    shouldBeOK(d)
  }
  test("should calculate diffs of prefixes") {
    val iri = IRI("http://example.org/")
    val x1 = Schema.empty.withPrefixMap(Some(PrefixMap(Map(Prefix(":") -> iri))))
    val x2 = Schema.empty.withPrefixMap(Some(PrefixMap(Map(Prefix(":") -> iri))))
    val d = ShExDiff.schemaDiff(x1, x2)
    shouldBeOK(d)
  }

  def shouldBeOK[A](x: ShExDiff.Result[A])(implicit loc: munit.Location): Unit =
    if (!x.isValid) {
      fail(s"Different: $x")
    }
}
