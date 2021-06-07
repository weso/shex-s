package es.weso.shextest.manifest
import java.nio.file.Paths
import com.typesafe.config.{Config, ConfigFactory}
import munit._

class RDF2ManifestSingleTest extends CatsEffectSuite with ValidateManifest {

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
         "1dotRefOR3_fail"
         ),
       List(),
       verbose = true).map(rs => assertEquals(rs.size > 0, true))
  }

}
