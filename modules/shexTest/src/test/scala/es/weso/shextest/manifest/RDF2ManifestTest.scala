package es.weso.shextest.manifest

// import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import cats.effect._
import cats.implicits._
import munit._
import ValidateManifest._
import TestSelector._
import es.weso.utils.VerboseLevel
import scala.concurrent.duration._
import es.weso.rdf.nodes.IRI
import java.nio.file.Path
import java.nio.file.Paths


class RDF2ManifestTest extends CatsEffectSuite {

  val conf: Config = ConfigFactory.load()
  val validationFolder = conf.getString("testsFolder")
  val assumeLocal: Option[(IRI,Path)] = Some((IRI("https://raw.githubusercontent.com/shexSpec/shexTest/master/"), Paths.get("src/test/resources/shexTest")))
  
  test("RDF2Manifest schemas") {
    checkResults(parseManifest("manifest", "schemas", validationFolder, 
      All, 
      List(), // "AND3G","Extend3G","ExtendANDExtend3GAND3G"),
      1.seconds,
      assumeLocal, VerboseLevel.Nothing)
    )
  }

  test("RDF2Manifest negativeSyntax") {
    checkResults(
      parseManifest("manifest", "negativeSyntax", 
        validationFolder, 
        All, 
        List(), // "1unknowndatatypeMaxInclusive"),
        1.seconds,
        assumeLocal, VerboseLevel.Info)
     )
  }

  test("RDF2Manifest negativeStructure") {
    checkResults(parseManifest(
      "manifest",
      "negativeStructure",
      validationFolder,
      All,
      List(), 
/*        "1MissingRef",
        "1focusMissingRefdot",
        "includeExpressionNotFound",
        "Cycle1Negation1",
        "Cycle1Negation2",
        "Cycle1Negation3",
        "TwoNegation",
        "Cycle2Negation",
        "Cycle2Extra" */
      1.seconds,
      assumeLocal, VerboseLevel.Nothing
    ))
  } 

  test("RDF2Manifest validating".only) {
    checkResults(parseManifest("manifest", 
       "validation", 
       validationFolder, 
       All,
       // Some("vitals-RESTRICTS-pass_lie-BP"),
       List(),
      1.seconds,
      assumeLocal, VerboseLevel.Nothing), false)
  }

  def checkResults(process: IO[List[Result]], verbose: Boolean = false): IO[Unit] = for { 
      results <- process
      failedValues = results.filter(_.isOk == false)
      _ <- IO.println(s"${failedValues.size}/${results.size} values failed")
      _ <- failedValues.map(fv => IO.println(s"Failed value: ${fv.name}\n${if (verbose) s"Reason: ${fv.reason}" else ""}")).sequence
  } yield assertEquals(failedValues.map(_.name),List())

}
