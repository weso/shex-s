package es.weso.wshex.wshex2es
import munit._
import cats._
import cats.implicits._
import es.weso.shex._
import es.weso.wshex._
import es.weso.utils.VerboseLevel
import es.weso.utils.test._

class WShEx2ESTest extends CatsEffectSuite {


   checkConversion(
    "P31_Q5",
    """|PREFIX :  <http://www.wikidata.org/entity/>
       |
       |<Human> {
       |  :P31 [ :Q5 ]  
       |}""".stripMargin,
    "WShExC",
    """|PREFIX wd:  <http://www.wikidata.org/entity/>
       |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
       |    
       |<Human> {
       | wdt:P31 [ wd:Q5 ]  
       |}""".stripMargin,
    "ShExC",
    VerboseLevel.Nothing
   ) 

    checkConversion(
    "P31_dot",
    """|PREFIX :  <http://www.wikidata.org/entity/>
       |
       |<Human> {
       |  :P31 . 
       |}""".stripMargin,
    "WShExC",
    """|PREFIX wd:  <http://www.wikidata.org/entity/>
       |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
       |    
       |<Human> {
       | wdt:P31 .
       |}""".stripMargin,
    "ShExC",
    VerboseLevel.Nothing
   ) 

   checkConversion(
    "EachOf_P31_dot",
    """|PREFIX :  <http://www.wikidata.org/entity/>
       |
       |<S> {
       |  :P31 . ;
       |  :P23 . 
       |}""".stripMargin,
    "WShExC",
    """|PREFIX wd:  <http://www.wikidata.org/entity/>
       |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
       |    
       |<S> {
       | wdt:P31 . ;
       | wdt:P23 . 
       |}""".stripMargin,
    "ShExC",
    VerboseLevel.Nothing
   ) 

   checkConversion(
    "OneOf_P31_dot",
    """|PREFIX :  <http://www.wikidata.org/entity/>
       |
       |<S> {
       |  :P31 . |
       |  :P23 . 
       |}""".stripMargin,
    "WShExC",
    """|PREFIX wd:  <http://www.wikidata.org/entity/>
       |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
       |    
       |<S> {
       | wdt:P31 . |
       | wdt:P23 . 
       |}""".stripMargin,
    "ShExC",
    VerboseLevel.Nothing
   )

   checkConversion(
    "Qualifiers",
    """|PREFIX :  <http://www.wikidata.org/entity/>
       |
       |<S> {
       |  :P31 [ :Q5 ] {| :P585 . |}
       |}""".stripMargin,
    "WShExC",
    """|PREFIX wd:  <http://www.wikidata.org/entity/>
       |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
       |PREFIX p:   <http://www.wikidata.org/prop/>
       |PREFIX ps:  <http://www.wikidata.org/prop/statement/>
       |PREFIX pq:  <http://www.wikidata.org/prop/qualifier/>
       |    
       |<S> {
       | wdt:P31 . ;
       | p:P31 { ps:P31 [ wd:Q5 ] ; pq:P585 . }
       |}""".stripMargin,
    "ShExC",
    VerboseLevel.Nothing,
    IgnoreTest
   )   

   checkConversion(
    "References",
    """|PREFIX :  <http://www.wikidata.org/entity/>
       |
       |<S> {
       |  :P31 [ :Q5 ] References {| :P248 . ; :P227 . |}
       |}""".stripMargin,
    "WShExC",
    """|PREFIX wd:  <http://www.wikidata.org/entity/>
       |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
       |PREFIX p:   <http://www.wikidata.org/prop/>
       |PREFIX ps:  <http://www.wikidata.org/prop/statement/>
       |PREFIX pq:  <http://www.wikidata.org/prop/qualifier/>
       |PREFIX pr: <http://www.wikidata.org/prop/reference/>
       |PREFIX prov: <http://www.w3.org/ns/prov#>
       |    
       |<S> {
       | wdt:P31 . ;
       | p:P31 { 
       |  ps:P31 [ wd:Q5 ] ; 
       |  prov:wasDerivedFrom {
       |   pr:P248 . ;
       |   pr:P227 .
       |  }
       | }
       |}""".stripMargin,
    "ShExC",
    VerboseLevel.Nothing,
    IgnoreTest
   )   


   checkConversion(
    "Empty",
    """|""".stripMargin,
    "WShExC",
    """|""".stripMargin,
    "ShExC",
    VerboseLevel.Nothing
   )

   
   def checkConversion(
      name: String,
      wShExStr: String,
      formatWShEx: String = "WShExC",
      expectedShExStr: String,
      expectedFormatShEx: String = "ShExC",
      verboseLevel: VerboseLevel, 
      ignore: ShouldIgnoreOption = DontIgnore
  )(implicit loc: munit.Location): Unit = if (ignore == IgnoreTest) {
   println(s"Ignored test: $name")
  } else {
    val convertOptions = WShEx2ESConvertOptions.default
    val entityIri = es.weso.wbmodel.Value.defaultIRI

    test(name) {
      Schema
        .fromString(expectedShExStr, expectedFormatShEx, None)
        .flatMap(shexSchemaExpected =>
          WSchema.parseFormat(formatWShEx)
          .flatMap(wshexFormat =>
          WSchema.fromString(wShExStr, wshexFormat, None, entityIri, verboseLevel)
          .map(wschema => 
            WShEx2ES(convertOptions).convert(wschema)
            .fold(
                err => fail(s"Error converting WShEx -> ShEx: $err"),
                shexSchemaConverted => 
                  if (shexSchemaConverted.toString != shexSchemaExpected.toString) {
                    println(s"""|Schemas are different
                                |wschema
                                |${wschema.shapes}
                                |converted: 
                                |${shexSchemaConverted.shapes.get}
                                |-----------endConverted
                                |expected: 
                                |${shexSchemaExpected.shapes.get}
                                |----------endExpected
                                |convertedString
                                |${shexSchemaConverted.toString}
                                |----------endConvertedString
                                |expectedString
                                |${shexSchemaExpected.toString}
                                |----------endExpectedString
                                |""".stripMargin)
                  }
                  assertEquals(
                        shexSchemaConverted.toString, 
                        shexSchemaExpected.toString)
            ))))
    }
  }
}
