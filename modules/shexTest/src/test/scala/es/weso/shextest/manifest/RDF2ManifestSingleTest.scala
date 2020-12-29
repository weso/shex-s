package es.weso.shextest.manifest
import java.nio.file.Paths
import com.typesafe.config.{Config, ConfigFactory}
import munit._

class RDF2ManifestSingleTest extends FunSuite with ValidateManifest {

  val conf: Config = ConfigFactory.load()
  val validationFolder = conf.getString("testsFolder")
  val shexFolderURI = Paths.get(validationFolder).normalize.toUri.toString

  test("RDF2Manifest") {
     parseManifest(
       "manifest", 
       "validation", 
       validationFolder, 
       // None, 
       Some(
         // "Extend3G-pass"
         "2EachInclude1-S2"
         ),
       List(),
       verbose = true)
  }

}
