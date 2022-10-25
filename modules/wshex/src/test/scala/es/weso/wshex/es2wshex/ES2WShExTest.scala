package es.weso.wshex.es2wshex
import munit._
import io.circe.Json
import es.weso.rdf.nodes.IRI
import es.weso.rdf.PrefixMap
import es.weso.rbe.interval.IntLimit
import es.weso.wbmodel._
import es.weso.utils.VerboseLevel._
import es.weso.shex.{Schema => ShExSchema}
import es.weso.wshex._
import es.weso.wshex.TermConstraint._
import es.weso.wshex.WNodeConstraint._
import es.weso.rbe.interval._
import es.weso.rdf.nodes._
import es.weso.utils.test._
import es.weso.utils.VerboseLevel

class ES2WShExTest extends CatsEffectSuite {

  checkConversion(
    "P31 .",
    """|PREFIX wd:  <http://www.wikidata.org/entity/>
       |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
       |PREFIX p:   <http://www.wikidata.org/prop/>
       |PREFIX ps:  <http://www.wikidata.org/prop/statement/>
       |PREFIX pq:  <http://www.wikidata.org/prop/qualifier/>
       |    
       |<S> {
       | wdt:P31 . ;
       |}""".stripMargin,
    "ShExC",
    """|PREFIX :  <http://www.wikidata.org/entity/>
       |
       |<S> {
       | :P31 .
       |}""".stripMargin,
    "WShExC",
    VerboseLevel.Nothing
   ) 

  checkConversion(
    "Ignore wasDerivedFrom",
    """|PREFIX wd:  <http://www.wikidata.org/entity/>
       |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
       |PREFIX p:   <http://www.wikidata.org/prop/>
       |PREFIX pr:  <http://www.wikidata.org/prop/reference/>
       |PREFIX prov: <http://www.w3.org/ns/prov#>
       |PREFIX ps:  <http://www.wikidata.org/prop/statement/>
       |PREFIX pq:  <http://www.wikidata.org/prop/qualifier/>
       |    
       |<S> {
       | prov:wasDerivedFrom @<T>
       |}
       |<T> {
       | pr:P248 .
       |}""".stripMargin,
    "ShExC",
    """|PREFIX prov: <http://www.w3.org/ns/prov#>
       |PREFIX :  <http://www.wikidata.org/entity/>
       |<S> {
       |}
       |<T> {
       |}""".stripMargin,
    "WShExC",
    VerboseLevel.Nothing
   )

checkConversion(
    "p:P31 . provenance",
    """|PREFIX wd:  <http://www.wikidata.org/entity/>
       |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
       |PREFIX p:   <http://www.wikidata.org/prop/>
       |PREFIX pr:  <http://www.wikidata.org/prop/reference/>
       |PREFIX prov: <http://www.w3.org/ns/prov#>
       |PREFIX ps:  <http://www.wikidata.org/prop/statement/>
       |PREFIX pq:  <http://www.wikidata.org/prop/qualifier/>
       |    
       |<S> {
       | p:P31 @<S1> 
       |}
       |
       |<S1> {
       | ps:P31 . ;
       | prov:wasDerivedFrom @<T> {2} ;
       |}
       |
       |<T> {
       | pr:P248 . 
       |}
       |""".stripMargin,
    "ShExC",
    """|PREFIX prov: <http://www.w3.org/ns/prov#>
       |PREFIX :  <http://www.wikidata.org/entity/>
       |
       |<S> {
       | :P31 . References {| :P248 . |} {2} ;
       |}
       |<S1> {
       |}
       |<T> { 
       |} """.stripMargin,
    "WShExC",
    VerboseLevel.Nothing
   )   

 checkConversion(
    "p:P31 . provenance with EachOf",
    """|PREFIX wd:  <http://www.wikidata.org/entity/>
       |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
       |PREFIX p:   <http://www.wikidata.org/prop/>
       |PREFIX pr:  <http://www.wikidata.org/prop/reference/>
       |PREFIX prov: <http://www.w3.org/ns/prov#>
       |PREFIX ps:  <http://www.wikidata.org/prop/statement/>
       |PREFIX pq:  <http://www.wikidata.org/prop/qualifier/>
       |    
       |<S> {
       | p:P31 @<S1> 
       |}
       |
       |<S1> {
       | ps:P31 . ;
       | prov:wasDerivedFrom @<T> {2} ;
       |}
       |
       |<T> {
       | pr:P248 [ wd:Q5 ] ;
       | pr:P218 .
       |}
       |""".stripMargin,
    "ShExC",
    """|PREFIX prov: <http://www.w3.org/ns/prov#>
       |PREFIX :  <http://www.wikidata.org/entity/>
       |
       |<S> {
       | :P31 . References 
       |          {| :P248 [ :Q5 ] ; :P218 . |} {2} 
       |}
       |<S1> {
       |}
       |<T> { 
       |} """.stripMargin,
    "WShExC",
    VerboseLevel.Nothing
   ) 

  checkConversion(
    "Empty",
    """|""".stripMargin,
    "ShExC",
    """|""".stripMargin,
    "WShExC",
    VerboseLevel.Nothing
   ) 

  def checkConversion(
      name: String,
      shExStr: String,
      formatShEx: String = "ShExC",
      expectedWShExStr: String,
      expectedFormatWShEx: String = "WShExC",
      verboseLevel: VerboseLevel, 
      ignore: ShouldIgnoreOption = DontIgnore
  )(implicit loc: munit.Location): Unit = if (ignore == Ignore) {
   println(s"Ignored test: $name")
  } else {
    val convertOptions = ES2WShExConvertOptions.default
    val entityIri = es.weso.wbmodel.Value.defaultIRI

    test(name) {
      WSchema.parseFormat(expectedFormatWShEx).flatMap(wshexFormat =>
      WSchema.fromString(expectedWShExStr, wshexFormat, None, entityIri, verboseLevel).flatMap(wshexSchemaExpected =>
      ShExSchema.fromString(shExStr, formatShEx, None).map(schema => 
      ES2WShEx(convertOptions).convert(schema).fold(
        err => fail(s"Error converting ShEx -> WShEx: $err"),
        wshexSchemaConverted => 
          if (wshexSchemaConverted.toString != wshexSchemaExpected.toString) {
                    println(s"""|Schemas are different
                                |schema
                                |${schema.shapes}
                                |converted: 
                                |${wshexSchemaConverted.shapes}
                                |-----------endConverted
                                |expected: 
                                |${wshexSchemaExpected.shapes}
                                |----------endExpected
                                |convertedString
                                |${wshexSchemaConverted.toString}
                                |----------endConvertedString
                                |expectedString
                                |${wshexSchemaExpected.toString}
                                |----------endExpectedString
                                |""".stripMargin)
                  }
                  assertEquals(
                        wshexSchemaConverted.toString, 
                        wshexSchemaExpected.toString)
            ))))
    }
  }

/*
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
                valueSet(List(EntityIdValueSetValue(ItemId("Q5", wd + "Q5")))),
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
              valueSet(List(EntityIdValueSetValue(ItemId("Q5", wd + "Q5")))),
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
          emptyExpr,
          1,
          IntOrUnbounded.fromInt(1),
          Some(
            QualifierSpec(
              EachOfPs(
                List(
                  PropertyLocal(
                    PropertyId.fromIRI(wd + "P407"),
                    valueSet(List(EntityIdValueSetValue(ItemId("Q1860", wd + "Q1860")))),
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
          emptyExpr,
          1,
          IntOrUnbounded.fromInt(1),
          Some(
            QualifierSpec(
              EachOfPs(
                List(
                  PropertyLocal(
                    PropertyId.fromIRI(wd + "P407"),
                    valueSet(List(EntityIdValueSetValue(ItemId("Q1860", wd + "Q1860")))),
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
              emptyExpr,
              1,
              IntOrUnbounded.fromInt(1),
              None
            ),
            TripleConstraintLocal(
              PropertyId.fromIRI(wd + "P407"),
              valueSet(List(EntityIdValueSetValue(ItemId("Q1860", wd + "Q1860")))),
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
          WShapeOr(Some(s), List(WShapeRef(None,t), WShapeRef(None,u))),
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
    val convertOptions = ES2WShExConvertOptions.default
    test(name) {
      ShExSchema
        .fromString(shexStr, format, None)
        .map(schema =>
          ES2WShEx(convertOptions)
            .convert(schema)
            .fold(
              err => fail(s"Error converting ShEx -> WShEx: ${err}"),
              wshexSchema => assertEquals(wshexSchema.toString, expected.toString)
            )
        )
    }
  } */
}
