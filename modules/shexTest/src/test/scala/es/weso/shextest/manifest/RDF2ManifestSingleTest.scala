package es.weso.shextest.manifest

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}

class RDF2ManifestSingleTest extends ValidateManifest {

  val conf: Config = ConfigFactory.load()
  val validationFolder = conf.getString("testsFolder")
  val shexFolderURI = Paths.get(validationFolder).normalize.toUri.toString

  describe("RDF2Manifest") {
     parseManifest(
       "manifest", 
       "validation", 
       validationFolder, 
       // None, 
       Some("Extend3G-pass"),
       List(),
       verbose = true)
  }

}
