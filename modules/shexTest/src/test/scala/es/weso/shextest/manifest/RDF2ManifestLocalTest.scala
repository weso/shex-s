package es.weso.shextest.manifest

// import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest._

class RDF2ManifestLocalTest extends FunSpec with ValidateManifest {

  val conf: Config = ConfigFactory.load()
  // val shexFolder = conf.getString("localFolderTest")
  // val shexFolderURI = Paths.get(shexFolder).normalize.toUri.toString

  describe("RDF2Manifest inheritance") {
    val localFolder = conf.getString("localTestsFolder")
    parseManifestValidating("manifest","inheritance", localFolder, None)
  } 

  describe("RDF2Manifest localTests") {
    val validationFolder = conf.getString("localTestsFolder")
    parseManifestValidating("manifest", "schemas", validationFolder, None)
  } 
}
