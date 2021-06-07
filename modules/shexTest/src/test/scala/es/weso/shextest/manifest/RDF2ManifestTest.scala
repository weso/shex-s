package es.weso.shextest.manifest

// import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import cats.effect._
import cats.implicits._
import munit._

class RDF2ManifestTest extends CatsEffectSuite with ValidateManifest {

  val conf: Config = ConfigFactory.load()
  val validationFolder = conf.getString("testsFolder")
    
  test("RDF2Manifest schemas") {
    checkResults(parseManifest("manifest", "schemas", validationFolder, 
      None, 
      List("AND3G","Extend3G","ExtendANDExtend3GAND3G"), 
      false)
    )
  }

  test("RDF2Manifest negativeSyntax") {
    checkResults(parseManifest("manifest", "negativeSyntax", validationFolder, None, List("1unknowndatatypeMaxInclusive"), true))
  }

  test("RDF2Manifest negativeStructure") {
    checkResults(parseManifest(
      "manifest",
      "negativeStructure",
      validationFolder,
      None,
      List(
        "1MissingRef",
        "1focusMissingRefdot",
        "includeExpressionNotFound",
        "Cycle1Negation1",
        "Cycle1Negation2",
        "Cycle1Negation3",
        "TwoNegation",
        "Cycle2Negation",
        "Cycle2Extra"
      ),
      false
    ))
  } 

  test("RDF2Manifest validating".ignore) {
    checkResults(parseManifest("manifest", 
       "validation", 
       validationFolder, 
       None,
       // Some("vitals-RESTRICTS-pass_lie-BP"),
       List(
         "startNoCode1_pass",
         "1dotNoCode1_pass",
/*         "extends-abstract-multi-empty_fail-Ref2ExtraP",
         "extends-abstract-multi-empty_fail-Ref1ExtraP",
         "extends-abstract-multi-empty_fail-ReferrerExtraP",
         "ANDAbstract-pass",
         "AND3G-pass",
         "ExtendAND3G-pass",
         "Extend3G-pass",
         "ExtendANDExtend3GAND3G-pass",
         "ExtendANDExtend3GAND3G-t33" 

1list0PlusDot-manualList_extraArc_Iv1,Iv2,Iv3_fail
[info]   extends-abstract-multi-empty_fail-Ref2ExtraP
[info]   extends-abstract-multi-empty_fail-Ref1ExtraP
[info]   extends-abstract-multi-empty_fail-ReferrerExtraP
[info]   AND3G-pass
[info]   ExtendAND3G-pass
[info]   Extend3G-pass
[info]   ExtendANDExtend3GAND3G-pass
[info]   ExtendANDExtend3GAND3G-t33

1list0PlusDot-manualList_extraArc_Iv1,Iv2,Iv3_fail
[info]   extends-abstract-multi-empty_fail-Ref2ExtraP
[info]   extends-abstract-multi-empty_fail-Ref1ExtraP
[info]   extends-abstract-multi-empty_fail-ReferrerExtraP
[info]   ExtendAND3G-pass
[info]   Extend3G-pass
[info]   ExtendANDExtend3GAND3G-pass
[info]   ExtendANDExtend3GAND3G-t33
         
         */
       ), 
       false))
  }

  def checkResults(process: IO[List[Result]]): IO[Unit] = for { 
      results <- process
      failedValues = results.filter(_.isOk == false)
      _ <- IO { println(s"${failedValues.size}/${results.size} values failed")}
      _ <- failedValues.map(fv => 
        IO { 
          println(s"Failed value: ${fv.name}\n Reason: ${fv.reason}")
        }
      ).sequence
  } yield assertEquals(failedValues.map(_.name),List())

}
