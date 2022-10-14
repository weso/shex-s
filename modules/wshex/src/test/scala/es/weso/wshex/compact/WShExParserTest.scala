package es.weso.wshex.compact
import munit._
import es.weso.wshex._
import io.circe.Json
import es.weso.rdf.nodes._
import es.weso.rdf.PrefixMap
import es.weso.rbe.interval.IntLimit
import es.weso.wbmodel.{Lang => WLang, _}
import es.weso.utils.VerboseLevel._
import es.weso.wshex.TermConstraint._
import WNodeConstraint._

class WShExParserTest extends CatsEffectSuite {

  val ex = IRI("http://example.org/")
  val p = IRI("http://www.wikidata.org/prop/")
  val ps = IRI("http://www.wikidata.org/prop/statement/")
  val wd = IRI("http://www.wikidata.org/entity/")
  val wdt = IRI("http://www.wikidata.org/prop/direct/")
  val pm: PrefixMap =
    PrefixMap.fromMap(Map("" -> wd))
  val s: ShapeLabel = IRILabel(IRI("S"))
  val labelS = Some(s)

  {
    val se: WShapeExpr = WShape(
      labelS,
      false,
      List(),
      Some(
        TripleConstraintLocal(
          PropertyId.fromIRI(wdt + "P31"),
          valueSet(List(EntityIdValueSetValue(ItemId("Q5", wd + "Q5")))),
          1,
          IntLimit(1)
        )
      ),
      List()
    )

    checkSchema(
      "local valueSetValue",
      s"""|prefix :    <${wd.str}>
          |<S> {
          | :P31 [ :Q5 ]
          |}
          |""".stripMargin,
      WSchema(shapesMap = Map(s -> se), prefixes = Some(pm))
    )
  }

  {
    val se: WShapeExpr = WShape(
      labelS,
      false,
      List(),
      Some(
        EachOf(exprs =
          List(
            TripleConstraintLocal(
              PropertyId.fromIRI(wdt + "P31"),
              valueSet(List(EntityIdValueSetValue(ItemId("Q5", wd + "Q5")))),
              1,
              IntLimit(1)
            ),
            TripleConstraintLocal(
              PropertyId.fromIRI(wdt + "P279"),
              emptyExpr,
              1,
              IntLimit(1)
            )
          )
        )
      ),
      List()
    )

    checkSchema(
      "local valueSetValue",
      s"""|prefix :    <${wd.str}>
          |<S> {
          | :P31 [ :Q5 ] ;
          | :P279 .
          |}
          |""".stripMargin,
      WSchema(shapesMap = Map(s -> se), prefixes = Some(pm))
    )
  }

  {
    val se: WShapeExpr = WShape(
      labelS,
      false,
      List(),
      Some(
        OneOf(exprs =
          List(
            TripleConstraintLocal(
              PropertyId.fromIRI(wdt + "P31"),
              valueSet(List(EntityIdValueSetValue(ItemId("Q5", wd + "Q5")))),
              1,
              IntLimit(1)
            ),
            TripleConstraintLocal(
              PropertyId.fromIRI(wdt + "P279"),
              emptyExpr,
              1,
              IntLimit(1)
            )
          )
        )
      ),
      List()
    )

    checkSchema(
      "OneOf",
      s"""|prefix :    <${wd.str}>
          |<S> {
          | :P31 [ :Q5 ] |
          | :P279 .
          |}
          |""".stripMargin,
      WSchema(shapesMap = Map(s -> se), prefixes = Some(pm))
    )
  }

  {
    val se: WShapeExpr = WShape(
      labelS,
      false,
      List(),
      Some(
        TripleConstraintLocal(
          PropertyId.fromIRI(wdt + "P31"),
          valueSet(List(EntityIdValueSetValue(ItemId("Q5", wd + "Q5")))),
          1,
          IntLimit(1),
          Some(
            QualifierSpec(
              PropertyLocal(
                PropertyId.fromIRI(wdt + "P580"),
                emptyExpr,
                1,
                IntLimit(1)
              ),
              false
            )
          )
        )
      ),
      List()
    )

    checkSchema(
      "simple qualifier",
      s"""|prefix :    <${wd.str}>
          |<S> {
          | :P31 [ :Q5 ] {| :P580 . |}
          |}
          |""".stripMargin,
      WSchema(shapesMap = Map(s -> se), prefixes = Some(pm))
    )
  }

  {
    val se: WShapeExpr =
      WShapeOr(
        labelS,
        List(
          WShape(
            None,
            false,
            List(),
            Some(
              TripleConstraintLocal(
                PropertyId.fromIRI(wdt + "P31"),
                valueSet(List(EntityIdValueSetValue(ItemId("Q5", wd + "Q5")))),
                1,
                IntLimit(1),
                None
              )
            ),
            List()
          ),
          WShape(
            None,
            false,
            List(),
            Some(
              TripleConstraintLocal(
                PropertyId.fromIRI(wdt + "P31"),
                valueSet(List(EntityIdValueSetValue(ItemId("Q6", wd + "Q6")))),
                1,
                IntLimit(1),
                None
              )
            ),
            List()
          )
        )
      )

    checkSchema(
      "simple OR",
      s"""|prefix :    <${wd.str}>
          |<S> {
          | :P31 [ :Q5 ]
          |} OR {
          | :P31 [ :Q6 ]
          |}
          |""".stripMargin,
      WSchema(shapesMap = Map(s -> se), prefixes = Some(pm))
    )
  }

  checkSchema(
    "Label any en",
    s"""|prefix :    <${wd.str}>
          |<S> Label ( en -> . ) {
          |}
          |""".stripMargin,
    WSchema(
      shapesMap = Map(
        s ->
          WShape(
            labelS,
            false,
            List(),
            None,
            List(LabelConstraint(Lang("en"), None))
          )
      ),
      prefixes = Some(pm)
    )
  )

  checkSchema(
    "Label any en with Extra",
    s"""|prefix :    <${wd.str}>
          |<S> EXTRA :P31 Label ( en -> . ) {
          | :P31 [ :Q5 ]
          |}
          |""".stripMargin,
    WSchema(
      shapesMap = Map(
        s ->
          WShape(
            labelS,
            false,
            List(PropertyId.fromIRI(wdt + "P31")),
            Some(
              TripleConstraintLocal(
                PropertyId.fromIRI(wdt + "P31"),
                valueSet(List(EntityIdValueSetValue(ItemId("Q5", wd + "Q5")))),
                1,
                IntLimit(1),
                None
              )
            ),
            List(LabelConstraint(Lang("en"), None))
          )
      ),
      prefixes = Some(pm)
    )
  )

  def checkSchema(
      name: String,
      shexStr: String,
      expected: WSchema,
      format: WShExFormat = WShExFormat.CompactWShExFormat
  )(implicit loc: munit.Location): Unit =
    test(name) {
      WSchema
        .fromString(schemaString = shexStr, format = format, base = None, verbose = Info)
        .map(it => assertEquals(it.toString, expected.toString))
    }
}
