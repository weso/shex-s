package es.weso.wshex
import munit._
import io.circe.Json
import es.weso.rdf.nodes.IRI
import es.weso.rdf.PrefixMap
import es.weso.rbe.interval.IntLimit
import es.weso.wbmodel.{Lang => WBLang, _}
import es.weso.utils.VerboseLevel._
import es.weso.shex.{Schema => ShExSchema}
import TermConstraint._
import es.weso.rbe.interval._
import es.weso.rdf.nodes._

class ES2WShExTest extends CatsEffectSuite {

  val ex = IRI("http://example.org/")
  val p = IRI("http://www.wikidata.org/prop/")
  val ps = IRI("http://www.wikidata.org/prop/statement/")
  val pq = IRI("http://www.wikidata.org/prop/qualifier/")
  val wd = IRI("http://www.wikidata.org/entity/")
  val wdt = IRI("http://www.wikidata.org/prop/direct/")
  val rdfs = IRI("http://www.w3.org/2000/01/rdf-schema#")
  val skos = IRI("http://www.w3.org/2004/02/skos/core#")

  val pm: PrefixMap =
    PrefixMap.fromMap(
      Map(
        "" -> ex,
        "wd" -> wd,
        "wdt" -> wdt,
        "p" -> p,
        "ps" -> ps,
        "pq" -> pq,
        "rdfs" -> rdfs,
        "skos" -> skos
      )
    )

  val pmStr = s"""|prefix :    <${ex.str}>
       |prefix wd:  <${wd.str}>
       |prefix wdt: <${wdt.str}>
       |prefix p:   <${p.str}>
       |prefix ps:  <${ps.str}>
       |prefix pq:  <${pq.str}>
       |prefix rdfs: <${rdfs.str}>
       |prefix skos: <${skos.str}>
       |""".stripMargin
  val s: ShapeLabel = IRILabel(IRI("S"))
  val t: ShapeLabel = IRILabel(IRI("T"))
  val u: ShapeLabel = IRILabel(IRI("U"))

  checkSchema(
    "local valueSetValue",
    s"""|$pmStr
        |<S> {
        | wdt:P31 [ wd:Q5 ]
        |}
        |""".stripMargin,
    WSchema(
      shapesMap = Map(
        s ->
          WShape(
            Some(s),
            false,
            List(),
            Some(
              TripleConstraintLocal(
                PropertyId.fromIRI(wd + "P31"),
                ValueSet(None, List(EntityIdValueSetValue(ItemId("Q5", wd + "Q5")))),
                1,
                IntOrUnbounded.fromInt(1)
              )
            ),
            List()
          )
      ),
      prefixes = Some(pm)
    )
  )

  checkSchema(
    "simple Reference",
    s"""|$pmStr
       |<S> {
       | p:P31 {
       |  ps:P31 [ wd:Q5 ] ;
       | }
       |}
       |""".stripMargin,
    WSchema(
      shapesMap = Map(
        s -> WShape(
          Some(s),
          false,
          List(),
          Some(
            TripleConstraintLocal(
              PropertyId.fromIRI(wd + "P31"),
              ValueSet(None, List(EntityIdValueSetValue(ItemId("Q5", wd + "Q5")))),
              1,
              IntLimit(1)
            )
          ),
          List()
        )
      ),
      prefixes = Some(pm)
    )
  )

  {
    val schemaStr =
      s"""|$pmStr
          |<S> {
          | p:P856 {
          |  ps:P856 .   ;
          |  pq:P407	[ wd:Q1860 ] ;
          | }
          |}
          |""".stripMargin
    val se: WShapeExpr = WShape(
      Some(s),
      false,
      List(),
      Some(
        TripleConstraintLocal(
          PropertyId.fromIRI(wd + "P856"),
          EmptyExpr,
          1,
          IntOrUnbounded.fromInt(1),
          Some(
            QualifierSpec(
              EachOfPs(
                List(
                  QualifierLocal(
                    PropertyId.fromIRI(wd + "P407"),
                    ValueSet(None, List(EntityIdValueSetValue(ItemId("Q1860", wd + "Q1860")))),
                    1,
                    IntOrUnbounded.fromInt(1)
                  )
                )
              ),
              false
            )
          )
        )
      ),
      List()
    )

    checkSchema(
      "Qualifiers",
      schemaStr,
      WSchema(shapesMap = Map(s -> se), prefixes = Some(pm))
    )
  }

  {
    val schemaStr =
      s"""|$pmStr
          |<S> {
          | p:P856 @<PS>
          |}
          |<PS> {
          |  ps:P856 .            ;
          |  pq:P407 [ wd:Q1860 ] ;
          |}
          |""".stripMargin
    val se: WShapeExpr = WShape(
      Some(s),
      false,
      List(),
      Some(
        TripleConstraintLocal(
          PropertyId.fromIRI(wd + "P856"),
          EmptyExpr,
          1,
          IntOrUnbounded.fromInt(1),
          Some(
            QualifierSpec(
              EachOfPs(
                List(
                  QualifierLocal(
                    PropertyId.fromIRI(wd + "P407"),
                    ValueSet(None, List(EntityIdValueSetValue(ItemId("Q1860", wd + "Q1860")))),
                    1,
                    IntOrUnbounded.fromInt(1)
                  )
                )
              ),
              false
            )
          )
        )
      ),
      List()
    )
    val psr: ShapeLabel = IRILabel(IRI("PS"))

    val pse: WShapeExpr = WShape(
      Some(psr),
      false,
      List(),
      Some(
        EachOf(
          exprs = List(
            TripleConstraintLocal(
              PropertyId.fromIRI(wd + "P856"),
              EmptyExpr,
              1,
              IntOrUnbounded.fromInt(1),
              None
            ),
            TripleConstraintLocal(
              PropertyId.fromIRI(wd + "P407"),
              ValueSet(None, List(EntityIdValueSetValue(ItemId("Q1860", wd + "Q1860")))),
              1,
              IntOrUnbounded.fromInt(1),
              None
            )
          )
        )
      ),
      List()
    )

    checkSchema(
      "Qualifiers with reference",
      schemaStr,
      WSchema(shapesMap = Map(s -> se, psr -> pse), prefixes = Some(pm))
    )
  }

  checkSchema(
    "local OR",
    s"""|$pmStr
       |<S> @<T> OR @<U>
       |<T> {}
       |<U> {}
       |""".stripMargin,
    WSchema(
      shapesMap = Map(
        s ->
          WShapeOr(Some(s), List(WShapeRef(t), WShapeRef(u))),
        t -> WShape(Some(t), false, List(), None, List()),
        u -> WShape(Some(u), false, List(), None, List())
      ),
      prefixes = Some(pm)
    )
  )

  checkSchema(
    "labels",
    s"""|$pmStr
        |<S> {
        | rdfs:label [ @en ]
        |}
        |""".stripMargin,
    WSchema(
      shapesMap = Map(
        s ->
          WShape(Some(s), false, List(), None, List(LabelConstraint(Lang("en"), None)))
      ),
      prefixes = Some(pm)
    )
  )

  checkSchema(
    "labels and aliases",
    s"""|$pmStr
        |<S> {
        | rdfs:label [ @en ] ;
        | skos:altLabel [ @en ] 
        |}
        |""".stripMargin,
    WSchema(
      shapesMap = Map(
        s ->
          WShape(
            Some(s),
            false,
            List(),
            None,
            List(
              LabelConstraint(Lang("en"), None),
              AliasConstraint(Lang("en"), None)
            )
          )
      ),
      prefixes = Some(pm)
    )
  )

  def checkSchema(
      name: String,
      shexStr: String,
      expected: WSchema,
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
