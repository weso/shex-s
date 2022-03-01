package es.weso.shextest.manifest

import com.typesafe.config.{Config, ConfigFactory}
import munit._
import ValidateManifest._
import TestSelector._
import es.weso.utils.VerboseLevel
import scala.concurrent.duration._

class RDF2ManifestLocalTest extends FunSuite {

  val conf: Config = ConfigFactory.load()

  /*  describe("RDF2Manifest inheritance") {
    val localFolder = conf.getString("localTestsFolder")
    parseManifestValidating("manifest","inheritance", localFolder, None)
  } */

  test("RDF2Manifest localTests") {
    val validationFolder = conf.getString("localTestsFolder")
    parseManifest("manifest", "schemas", validationFolder, All, List(), 1.seconds, VerboseLevel.Nothing)
  }
}
