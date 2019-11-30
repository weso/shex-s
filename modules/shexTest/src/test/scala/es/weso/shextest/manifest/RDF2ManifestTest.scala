package es.weso.shextest.manifest

// import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest._

class RDF2ManifestTest extends FunSpec with ValidateManifest {

  val conf: Config = ConfigFactory.load()

  describe("RDF2Manifest schemas") {
    val validationFolder = conf.getString("testsFolder")
    parseManifestValidating("manifest", "schemas", validationFolder, None)
  }

  describe("RDF2Manifest negativeStructure") {
    val validationFolder = conf.getString("testsFolder")
    parseManifestValidating("manifest", "negativeStructure", validationFolder, None)
  }

  describe("RDF2Manifest negativeSyntax") {
    val validationFolder = conf.getString("testsFolder")
    parseManifestValidating("manifest", "negativeSyntax", validationFolder, None)
  } 

  describe("RDF2Manifest validating") {
    val validationFolder = conf.getString("testsFolder")
    parseManifestValidating("manifest", "validation", validationFolder, None)
  } 
  
}
