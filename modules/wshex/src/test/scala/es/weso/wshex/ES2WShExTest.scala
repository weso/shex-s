package es.weso.wshex
import munit._
import io.circe.Json
import es.weso.rdf.nodes.IRI
import es.weso.rdf.PrefixMap
import es.weso.rbe.interval.IntLimit
import es.weso.wbmodel.EntityId
import es.weso.wbmodel.PropertyId
import es.weso.utils.VerboseLevel._
import es.weso.shex.{Schema => ShExSchema}
import es.weso.rbe.interval._

class ES2WShExTest extends CatsEffectSuite {

  val ex = IRI("http://example.org/")
  val p = IRI("http://www.wikidata.org/prop/")
  val ps = IRI("http://www.wikidata.org/prop/statement/")
  val pq = IRI("http://www.wikidata.org/prop/qualifier/")
  val wd = IRI("http://www.wikidata.org/entity/")
  val wdt = IRI("http://www.wikidata.org/prop/direct/")
  val pm: PrefixMap =
    PrefixMap.fromMap(Map("" -> ex, "wd" -> wd, "wdt" -> wdt, "p" -> p, "ps" -> ps, "pq" -> pq))
  val s: ShapeLabel = IRILabel(IRI("S"))

  {
    val se: ShapeExpr = Shape(
      Some(s),
      false,
      List(),
      Some(
        TripleConstraintLocal(
          PropertyId.fromIRI(wdt + "P31"),
          ValueSet(None, List(EntityIdValueSetValue(EntityId.fromIri(wd + "Q5")))),
          1,
          IntOrUnbounded.fromInt(1)
        )
      )
    )

    checkSchema(
      "local valueSetValue",
      s"""|prefix :    <${ex.str}>
       |prefix wd:  <${wd.str}>
       |prefix wdt: <${wdt.str}>
       |prefix p:   <${p.str}>
       |prefix ps:  <${ps.str}>
       |prefix pq:  <${pq.str}>
       |<S> {
       | wdt:P31 [ wd:Q5 ]
       |}
       |""".stripMargin,
      Schema(Map(s -> se), None, pm)
    )
  }

  {
    val se: ShapeExpr = Shape(
      Some(s),
      false,
      List(),
      Some(
        TripleConstraintLocal(
          PropertyId.fromIRI(wdt + "P31"),
          ValueSet(None, List(EntityIdValueSetValue(EntityId.fromIri(wd + "Q5")))),
          1,
          IntLimit(1)
        )
      )
    )

    checkSchema(
      "simple Reference",
      s"""|prefix :    <${ex.str}>
       |prefix wd:  <${wd.str}>
       |prefix wdt: <${wdt.str}>
       |prefix p:   <${p.str}>
       |prefix ps:  <${ps.str}>
       |prefix pq:  <${pq.str}>
       |<S> {
       | p:P31 {
       |  ps:P31 [ wd:Q5 ] ;
       | }
       |}
       |""".stripMargin,
      Schema(Map(s -> se), None, pm)
    )
  }

  {
    val schemaStr =
      s"""|prefix :    <${ex.str}>
          |prefix wd:  <${wd.str}>
          |prefix pq:  <${pq.str}>
          |prefix wdt: <${wdt.str}>
          |prefix p:   <${p.str}>
          |prefix ps:  <${ps.str}>
          |<S> {
          | p:P856 {
          |  ps:P856 .   ;
          |  pq:P407	[ wd:Q1860 ] ;	 
          | }
          |}
          |""".stripMargin
    val se: ShapeExpr = Shape(
      Some(s),
      false,
      List(),
      Some(
        TripleConstraintLocal(
          PropertyId.fromIRI(wdt + "P856"),
          EmptyExpr,
          1,
          IntOrUnbounded.fromInt(1),
          Some(
            QualifierSpec(
              EachOfPs(
                List(
                  QualifierLocal(
                    PropertyId.fromIRI(wdt + "P407"),
                    ValueSet(None, List(EntityIdValueSetValue(EntityId.fromIri(wd + "Q1860")))),
                    1,
                    IntOrUnbounded.fromInt(1)
                  )
                )
              ),
              false
            )
          )
        )
      )
    )

    checkSchema(
      "Qualifiers",
      schemaStr,
      Schema(Map(s -> se), None, pm)
    )
  }

  def checkSchema(
      name: String,
      shexStr: String,
      expected: Schema,
      format: String = "ShExC"
  )(implicit loc: munit.Location): Unit = {
    val convertOptions = ESConvertOptions.default
    test(name) {
      ShExSchema
        .fromString(shexStr, format, None)
        .map(schema =>
          ES2WShEx(convertOptions)
            .convertSchema(schema)
            .fold(
              err => fail(s"Error converting ShEx -> WShEx: ${err}"),
              wshexSchema => assertEquals(wshexSchema.toString, expected.toString)
            )
        )
    }
  }
}
