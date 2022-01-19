package es.weso.shextest.manifest

import com.typesafe.config.{Config, ConfigFactory}
import munit._
import ValidateManifest._

class RDF2ManifestLocalTest extends FunSuite {

  val conf: Config = ConfigFactory.load()

  /*  describe("RDF2Manifest inheritance") {
    val localFolder = conf.getString("localTestsFolder")
    parseManifestValidating("manifest","inheritance", localFolder, None)
  } */

  test("RDF2Manifest localTests") {
    val validationFolder = conf.getString("localTestsFolder")
    parseManifest("manifest", "schemas", validationFolder, None, List(), false)
  }
}
