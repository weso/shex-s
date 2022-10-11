package es.weso.wshex.wshex2es
import munit._
import cats._
import cats.implicits._
import es.weso.shex._
import es.weso.wshex._
import es.weso.utils.VerboseLevel

class WShEx2ESTest extends CatsEffectSuite {

   checkConversion(
    "Simple example",
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
      verboseLevel: VerboseLevel
  )(implicit loc: munit.Location): Unit = {
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
                    assertEquals(
                        shexSchemaConverted.toString, 
                        shexSchemaExpected.toString)
            ))))
    }
  }
}
