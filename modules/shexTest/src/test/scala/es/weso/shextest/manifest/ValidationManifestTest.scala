package es.weso.shextest.manifest

import java.nio.file.Paths
import com.typesafe.config.{Config, ConfigFactory}
import cats.implicits._
import cats.effect.IO
import munit._
import es.weso.utils.testsuite._
import scala.concurrent.duration._
import es.weso.utils.FileUtils._
import es.weso.utils.VerboseLevel

class ValidationManifestTest extends CatsEffectSuite {

  val timeoutByTest = 15.seconds

  override def munitTimeout: Duration = 5.minutes

  // If the following variable is None, it runs all tests
  // Otherwise, it runs only the test whose name is equal to the value of this variable
  val nameIfSingle: Option[String] =
//     Some("ExtendANDExtend3GAND3G-t23")
    None

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("validationFolder")
//  val shexFolder = conf.getString("shexLocalFolder")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri

  test("run all") {

   val except: List[TestId] = 
     List().map(TestId(_))

   val cmp: IO[TestResults] = for {
      manifest <- RDF2Manifest.read(Paths.get(shexFolder + "/" + "manifest.ttl"), "Turtle", Some(shexFolderURI.toString), false)
      testSuite = manifest.toTestSuite(shexFolderURI, VerboseLevel.Nothing)
      ioresults <- testSuite.runAll(
        TestConfig.initial.copy(maxTimePerTest = timeoutByTest), 
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
     })
  }

  def showFailedResults(results: TestResults): String = 
   results.failed.map(_.entry.id.show).mkString(",\n")

  test("single") {
    val cmp: IO[TestResult] = for {
      manifest <- RDF2Manifest.read(Paths.get(shexFolder + "/" + "manifest.ttl"), "Turtle", Some(shexFolderURI.toString), false)
      testSuite = manifest.toTestSuite(shexFolderURI, VerboseLevel.Debug)
      result <- testSuite.runSingle(TestId("vitals-RESTRICTS-fail_sit-ReclinedVital"), TestConfig(timeoutByTest, true))
    } yield result
    cmp.map(res => {
      assertEquals(res.passed, true)
     }
    )
  }
  
}