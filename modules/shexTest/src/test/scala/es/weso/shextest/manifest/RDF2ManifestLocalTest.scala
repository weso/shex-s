package es.weso.shextest.manifest

import com.typesafe.config.{Config, ConfigFactory}
import munit._
import ValidateManifest._
import TestSelector._
import es.weso.utils.VerboseLevel
import scala.concurrent.duration._
import es.weso.rdf.nodes.IRI
import java.nio.file._
import es.weso.shex.validator.Validator

class RDF2ManifestLocalTest extends FunSuite {

  val conf: Config = ConfigFactory.load()
  val assumeLocal: Option[(IRI, Path)] = Some(
    (IRI("https://raw.githubusercontent.com/shexSpec/shexTest/master/"), Paths.get("src/test/resources/shexTest"))
  )

  test("RDF2Manifest localTests") {
    val validationFolder = conf.getString("localTestsFolder")
    parseManifest(
      "manifest",
      "schemas",
      validationFolder,
      All,
      List(),
      Validator.apply,
      1.seconds,
      assumeLocal,
      VerboseLevel.Nothing
    )
  }
}
