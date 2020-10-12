package es.weso.shextest.manifest

// import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.funspec.AnyFunSpec

class RDF2ManifestTest extends AnyFunSpec with ValidateManifest {

  val conf: Config = ConfigFactory.load()

/*  describe("RDF2Manifest schemas") {
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
  } */

  describe("RDF2Manifest validating") {
    val validationFolder = conf.getString("testsFolder")
    parseManifest("manifest", 
       "validation", 
       validationFolder, 
       // None,
       Some("startCode1fail_abort"),
       List(), 
       false)
  }

}
