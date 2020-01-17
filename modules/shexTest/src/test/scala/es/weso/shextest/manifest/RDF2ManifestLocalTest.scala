package es.weso.shextest.manifest

// import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.funspec.AnyFunSpec

class RDF2ManifestLocalTest extends AnyFunSpec with ValidateManifest {

  val conf: Config = ConfigFactory.load()

  /*  describe("RDF2Manifest inheritance") {
    val localFolder = conf.getString("localTestsFolder")
    parseManifestValidating("manifest","inheritance", localFolder, None)
  } */

  describe("RDF2Manifest localTests") {
    val validationFolder = conf.getString("localTestsFolder")
    parseManifest("manifest", "schemas", validationFolder, None, List(), false)
  }
}
