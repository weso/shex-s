package es.weso.wshex
import munit._
import io.circe.Json
import es.weso.rdf.nodes.IRI
import es.weso.rdf.PrefixMap
import es.weso.rbe.interval.IntLimit
import es.weso.wbmodel._
import es.weso.utils.VerboseLevel._

class ShEx2WShExTest extends CatsEffectSuite {

  val ex = IRI("http://example.org/")
  val p = IRI("http://www.wikidata.org/prop/")
  val ps = IRI("http://www.wikidata.org/prop/statement/")
  val wd = IRI("http://www.wikidata.org/entity/")
  val wdt = IRI("http://www.wikidata.org/prop/direct/")
  val pm: PrefixMap =
    PrefixMap.fromMap(Map("" -> ex, "wd" -> wd, "wdt" -> wdt, "p" -> p, "ps" -> ps))
  val s: ShapeLabel = IRILabel(IRI("S"))

  {
    val se: WShapeExpr = WShape(
      Some(s),
      false,
      List(),
      Some(
        TripleConstraintLocal(
          PropertyId.fromIRI(wdt + "P31"),
          ValueSet(None, List(EntityIdValueSetValue(ItemId("Q5",wd + "Q5")))),
          1,
          IntLimit(1)
        )
      ),
      List()
    )

    checkSchema(
      "local valueSetValue",
      s"""|prefix :    <${ex.str}>
       |prefix wd:  <${wd.str}>
       |prefix wdt: <${wdt.str}>
       |prefix p:   <${p.str}>
       |prefix ps:  <${ps.str}>
       |<S> {
       | wdt:P31 [ wd:Q5 ]
       |}
       |""".stripMargin,
      WSchema(shapesMap = Map(s -> se), prefixes = Some(pm))
    )
  }

  /* {
  val se: ShapeExpr = Shape(Some(s), false, List(),
    Some(TripleConstraintLocal(
      PropertyId.fromIRI(wdt + "P31"),
      ValueSet(None, List(EntityIdValueSetValue(EntityId.fromIri(wd+"Q5")))), 1,IntLimit(1)
   )))

 checkSchema("simple Reference",
   s"""|prefix :    <${ex.str}>
       |prefix wd:  <${wd.str}>
       |prefix wdt: <${wdt.str}>
       |prefix p:   <${p.str}>
       |prefix ps:  <${ps.str}>
       |<S> {
       | p:P31 {
       |  ps:P31 [ wd:Q5 ] ;
       |
       | }
       |}
       |""".stripMargin,
    Schema(Map(s -> se), None, pm))
 } */

  def checkSchema(
      name: String,
      shexStr: String,
      expected: WSchema,
      format: WShExFormat = WShExFormat.ESCompactFormat
  )(implicit loc: munit.Location): Unit =
    test(name) {
      WSchema
        .fromString(schemaString = shexStr, format = format, base = None, verbose = Info)
        .map(it => assertEquals(it.toString, expected.toString))
    }
}
