package es.weso.shextest.manifest

// import java.net.URI

// import es.weso.shex.implicits.decoderShEx.decodeSchema
// import es.weso.utils.IOUtils.fromES
// import io.circe.{Decoder, Json}
//import es.weso.utils.UriUtils._
import java.nio.file.Paths
import com.typesafe.config.{Config, ConfigFactory}
// import es.weso.rdf.PrefixMap
// import es.weso.rdf.jena.RDFAsJenaModel
// import es.weso.rdf.nodes.{BNode, IRI}
// import es.weso.shapemaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMap, _}
// import es.weso.shex._
// import es.weso.shex.validator.{ExternalIRIResolver, Validator}
// import es.weso.shapemaps._
// import es.weso.shex.compact.CompareSchemas
//import es.weso.shextest.manifest.Utils._
// import es.weso.shex.implicits.decoderShEx._
//import es.weso.shex.implicits.encoderShEx._
//import cats._
//import cats.data.EitherT
import cats.implicits._
import cats.effect.IO
//import ManifestPrefixes._
//import scala.io._
//import io.circe.parser._
//import io.circe.syntax._
import munit._
//import cats.effect.unsafe.IORuntime
import es.weso.utils.testsuite._
import scala.concurrent.duration._
// import cats.effect.kernel.Outcome
import es.weso.utils.FileUtils._

class ValidationManifestTest extends CatsEffectSuite with ValidateManifest {

  override def munitTimeout: Duration = 5.second

  // If the following variable is None, it runs all tests
  // Otherwise, it runs only the test whose name is equal to the value of this variable
  val nameIfSingle: Option[String] =
     Some("ExtendANDExtend3GAND3G-t23")
//     None

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("validationFolder")
//  val shexFolder = conf.getString("shexLocalFolder")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri

  // val ior = implicitly[IORuntime] // = cats.effect.unsafe.IORuntime.global

  test("run all") {

    val except: List[TestId] = 
     List(
"vitals-RESTRICTS-pass_lie-PostureVital",
"ExtendANDExtend3GAND3G-t28",
"vitals-RESTRICTS-pass_lie-BP",
"vitals-RESTRICTS-pass_sit-PostureVital",
"ExtendAND3G-pass",
"vitals-RESTRICTS-pass_sit-BP",
"ExtendANDExtend3GAND3G-t27",
"Extend3G-pass",
"ExtendANDExtend3GAND3G-t35",
"ExtendANDExtend3GAND3G-t26",
"extends-abstract-multi-empty_fail-ReferrerExtraP",
"ExtendANDExtend3GAND3G-t24",
"ExtendANDExtend3GAND3G-t34",
"ExtendANDExtend3GAND3G-t33",
"vitals-RESTRICTS-pass_lie-ReclinedBP",
"vitals-RESTRICTS-pass_lie-Posture",
"vitals-RESTRICTS-pass_lie-Vital",
"ANDAbstract-pass",
"vitals-RESTRICTS-pass_sit-Vital",
"AND3G-pass",
"vitals-RESTRICTS-pass_sit-Posture",
"extends-abstract-multi-empty_fail-Ref2ExtraP",
"vitals-RESTRICTS-pass_lie-PostureBP",
"vitals-RESTRICTS-pass_lie-Reclined",
"1list0PlusDot-manualList_extraArc_Iv1,Iv2,Iv3_fail",
"vitals-RESTRICTS-pass_lie-ReclinedVital",
"ExtendANDExtend3GAND3G-t32",
"ExtendANDExtend3GAND3G-pass",
"ExtendANDExtend3GAND3G-t31",
"vitals-RESTRICTS-fail_sit-ReclinedVital",
"vitals-RESTRICTS-pass_sit-PostureBP",
"extends-abstract-multi-empty_fail-Ref1ExtraP",
"Extend3G-t17",
"ExtendANDExtend3GAND3G-t30",
"vitals-RESTRICTS-fail_sit-ReclinedBP",
"extends-abstract-multi-empty_pass-missingOptRef1",
"Extend3G-t20",
"ExtendANDExtend3GAND3G-t25",
"extends-abstract-multi-empty_pass",
"node_kind_example" // Pending to parse result shape maps from TestSuite
).map(TestId(_))

    // val xs: OutcomeIO[TestResults] = ??? 
    val cmp: IO[TestResults] = for {
      manifest <- RDF2Manifest.read(Paths.get(shexFolder + "/" + "manifest.ttl"), "Turtle", Some(shexFolderURI.toString), false)
      testSuite = manifest.toTestSuite(shexFolderURI, false)
      ioresults <- testSuite.runAll(
        TestConfig.initial.copy(maxTimePerTest = 3 seconds), 
        except
      ).background.use(res => res.flatMap(_.fold(
       IO.raiseError(new RuntimeException(s"Cancelled")),
       IO.raiseError(_),
       IO(_) 
      )))
      results <- ioresults
      msg = s"""|After run all. ${testSuite.tests.length} total tests = ${results.passed.length} passed + ${results.failed.length} failed + ${results.skipped.length} skipped.
                |Failed:${results.failed.length}\n${showFailedResults(results)}
                |Not Found in Except: ${results.notFound.map(_.id).mkString(",")}
                |""".stripMargin
      _ <- writeFile("target/testResults.txt", msg)
      _ <- IO.println(s"After run all: ${results.passed.length}/${testSuite.tests.length}")
      _ <- IO.println(s"Failed: ${results.failed.length}\n${results.failed.map(_.show).mkString("\n")}")
    } yield results

    cmp.map(vs => {
      assertEquals(vs.failed, Vector[FailedResult]())
     }
    )
  }

  def showFailedResults(results: TestResults): String = 
   results.failed.map(_.entry.id.show).mkString(",\n")

    // val ior = implicitly[IORuntime] // = cats.effect.unsafe.IORuntime.global
  test("single") {
    val cmp: IO[TestResult] = for {
      manifest <- RDF2Manifest.read(Paths.get(shexFolder + "/" + "manifest.ttl"), "Turtle", Some(shexFolderURI.toString), false)
      testSuite = manifest.toTestSuite(shexFolderURI, true)
      // _ <- IO.println(s"Tests: ${testSuite.tests.map(_.id).mkString("\n")}")
      result <- testSuite.runSingle(TestId("startNoCode1_pass"),TestConfig.initial)
      _ <- IO.println(s"Result: ${result}")
    } yield result
    cmp.map(res => {
      assertEquals(res.passed, true)
     }
    )
  }
  
}

