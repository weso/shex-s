package es.weso.shexs

import java.nio.file.Path
import es.weso.shextest.manifest.TestSelector
import com.monovore.decline.Opts
import cats.implicits._
import es.weso.shextest.manifest.ValidateManifest._
import cats.effect.IO
import cats.effect.ExitCode
import es.weso.shextest.manifest.Result
import VerboseLevelOpt._
import es.weso.shextest.manifest._
import es.weso.utils.VerboseLevel
import scala.concurrent.duration._
import es.weso.rdf.nodes.IRI
import java.nio.file.Paths

case class Manifest(
    manifestFileName: String,
    parentPath: String,
    testsFolderPath: Path,
    testName: TestSelector,
    timeout: FiniteDuration,
    verbose: VerboseLevel
) {

  def run(): IO[ExitCode] = {
    val assumeLocal: Option[(IRI, Path)] = Some(
      (IRI("https://raw.githubusercontent.com/shexSpec/shexTest/master/"), Paths.get("src/test/resources/shexTest"))
    )
    parseManifest(
      manifestFileName,
      parentPath,
      testsFolderPath.toString,
      testName,
      List(),
      timeout,
      assumeLocal,
      verbose
    ).flatMap(results =>
      printResults(results) *>
        ExitCode.Success.pure
    )
  }

  private def printResults(results: List[Result]): IO[Unit] =
    IO.println(s"Failed/Total: ${results.filter(!_.isOk).length}/${results.length}") *>
      (if (verbose == VerboseLevel.Nothing) {
         IO.pure(())
       } else {
         results.map(printResult).sequence.map(_ => ())
       })

  private def printResult(result: Result): IO[Unit] =
    if (verbose >= VerboseLevel.All) IO.println(result)
    else if (result.isOk) IO.pure(())
    else IO.println(result.name)

}

object Manifest {

  lazy val manifestFileName =
    Opts.option[String]("manifest", short = "m", help = "Name of manifest file, e.g. \"manifest\".")

  lazy val parentPath =
    Opts.option[String]("parent-path", short = "p", help = "parent path, e. g. \"schemas\"")

  lazy val testsFolder =
    Opts.option[Path]("tests-folder", short = "t", help = "tests folder")

  lazy val testName: Opts[TestSelector] =
    Opts
      .option[String]("test-selector", short = "n", help = "Test selector (if none provided, it will run all")
      .orNone
      .map(TestSelector.fromOption(_))

  lazy val timeout: Opts[FiniteDuration] =
    Opts
      .option[Long]("timeout", short = "d", help = "Timeout duration (if none provided, 1 second")
      .withDefault(1L)
      .map(d => FiniteDuration(d, SECONDS))

  lazy val manifestCommand: Opts[Manifest] =
    Opts.subcommand("manifest", "Run manifest file containing tests") {
      (manifestFileName, parentPath, testsFolder, testName, timeout, verboseLevel)
        .mapN(Manifest.apply)
    }

}
