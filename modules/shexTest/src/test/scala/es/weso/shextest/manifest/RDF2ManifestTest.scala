package es.weso.shextest.manifest

// import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.funspec.AnyFunSpec

class RDF2ManifestTest extends AnyFunSpec with ValidateManifest {

  val conf: Config = ConfigFactory.load()

  describe("RDF2Manifest schemas") {
    val validationFolder = conf.getString("testsFolder")
    parseManifest("manifest", "schemas", validationFolder, None, List(), false)
  }

  describe("RDF2Manifest negativeSyntax") {
    val validationFolder = conf.getString("testsFolder")
    parseManifest("manifest", "negativeSyntax", validationFolder, None, List("1unknowndatatypeMaxInclusive"), true)
  }

  describe("RDF2Manifest negativeStructure") {
    val validationFolder = conf.getString("testsFolder")
    parseManifest(
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
    )
  } 

  describe("RDF2Manifest validating") {
    val validationFolder = conf.getString("testsFolder")
    parseManifest("manifest", 
       "validation", 
       validationFolder, 
       None,
       // Some("vitals-RESTRICTS-pass_lie-BP"),
       List(
         "startNoCode1_pass",
         "1dotNoCode1_pass",
        /* "vitals-RESTRICTS-pass_lie-Vital", // Ignored because it shouldn't conform as Vital is abstract
         "extends-abstract-multi-empty_pass",
         "ExtendsRepeatedP-pass",
         "AND3G-pass",
         "ExtendAND3G-pass",
         "Extend3G-pass",
         "ExtendANDExtend3GAND3G-pass",
         "ExtendANDExtend3GAND3G-t33",
         "vitals-RESTRICTS-pass_lie-BP",
         "vitals-RESTRICTS-pass_lie-PostureVital",
         "vitals-RESTRICTS-pass_lie-ReclinedVital",
         "vitals-RESTRICTS-pass_lie-PostureBP",
         "vitals-RESTRICTS-pass_lie-ReclinedBP",
         "vitals-RESTRICTS-pass_lie-Posture",
         "vitals-RESTRICTS-pass_lie-Reclined",
         "vitals-RESTRICTS-pass_sit-Vital",
         "vitals-RESTRICTS-pass_sit-BP",
         "vitals-RESTRICTS-pass_sit-PostureVital",
         "vitals-RESTRICTS-pass_sit-PostureBP",
         "vitals-RESTRICTS-pass_sit-Posture" */
       ), 
       false)
  }

}
