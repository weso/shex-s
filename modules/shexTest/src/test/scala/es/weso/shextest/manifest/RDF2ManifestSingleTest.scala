package es.weso.shextest.manifest
import java.nio.file.Paths
import com.typesafe.config.{Config, ConfigFactory}
import munit.{Only => _, _}
import ValidateManifest._
import TestSelector.Only
import es.weso.utils.VerboseLevel
import scala.concurrent.duration._
import es.weso.rdf.nodes.IRI
import java.nio.file._
import es.weso.shex.validator.Validator


class RDF2ManifestSingleTest extends CatsEffectSuite {

  val conf: Config = ConfigFactory.load()
  val validationFolder = conf.getString("testsFolder")
  val shexFolderURI = Paths.get(validationFolder).normalize.toUri.toString
  val assumeLocal: Option[(IRI,Path)] = Some((IRI("https://raw.githubusercontent.com/shexSpec/shexTest/master/"), Paths.get("src/test/resources/shexTest")))
  
  test("RDF2Manifest") {
     parseManifest(
       "manifest", 
       "validation", 
       validationFolder, 
       // None, 
       Only(
         // "Extend3G-pass"
         "vitals-RESTRICTS-pass_lie-BP"
         ),
       List(),
       Validator.apply, 
       1.seconds,
       assumeLocal,
       VerboseLevel.Nothing).map(rs => assertEquals(rs.size > 0, true))
  }

}
