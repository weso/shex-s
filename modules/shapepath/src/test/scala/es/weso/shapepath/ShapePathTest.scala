package es.weso.shapepath
import cats.MonadError
import cats.effect.{IO, Resource}
import es.weso.rdf.nodes.IRI
import es.weso.shex.{IRILabel, Schema}
import org.scalatest._
import matchers.should._
import org.scalatest.funspec.AnyFunSpec
import io.circe._
import io.circe.syntax._
import io.circe.parser._
// import cats.syntax.applicative._
import cats.implicits._
import scala.io.{BufferedSource, Source}
import Value._

trait ShapePathTest extends AnyFunSpec with Matchers {

  val manifestPath = "modules/shapepath/src/test/resources/test-suite/"

  def readFile(path: String): Resource[IO, BufferedSource] =
    Resource.fromAutoCloseable(IO(Source.fromFile(path)))

  def readContents(path: String): Resource[IO, String] =
    readFile(path).map(_.mkString)

  def readJsonContents(path: String): Resource[IO, Either[ParsingFailure, Json]] =
    readContents(path).map(parse(_))

  def json2manifest(json: Json): Either[String, Manifest] = {
    import Manifest._
    json.as[Manifest].fold(err => Left(s"Error converting Json file to Manifest: ${err.getMessage()}"), Right(_))
  }

  def either2io[E, A](e: Either[E, A], cnv: E => Throwable): IO[A] = {
    val either: Either[Throwable, A] = e.bimap(s => cnv(s), identity)
    MonadError[IO, Throwable].rethrow(IO(either))
  }

  def eitherStr2io[A](e: Either[String, A]): IO[A] =
    either2io(e, cnvMsg)

  def cnvMsg(msg: String): Throwable = new RuntimeException(msg)

  def cnvFailure(df: ParsingFailure): Throwable = new RuntimeException(df.getMessage())

  def readContents2(path1: String, path2: String): Resource[IO, (String,String)] =
    (readContents(path1), readContents(path2)).tupled

  def processEntryPositive(name: String, from: String, shapePath: String, expect: String): Unit = {
    val cmp = readContents2(manifestPath + from, manifestPath + expect + ".json").use(
      pair => {
        val (schemaStr,expectStr) = pair
        for {
          schema <- Schema.fromString(schemaStr, "ShEXJ", Some(IRI("base:/")))
          shapePath <- eitherStr2io(ShapePath.fromString(shapePath, "Compact", Some(IRI("base:/"))))
          pair = ShapePath.eval(shapePath, schema)
          expectedValues <- either2io(parse(expectStr), cnvFailure)
          (es, v) = pair
          _ <- if (!es.isEmpty) IO {
            println(s"Processing errors: ${es.map(_.toString + "\n").mkString}")
          } else IO.pure(())
        } yield (v,expectedValues)
      }
    )
    cmp.attempt
      .unsafeRunSync()
      .fold(
        err => fail(s"Error $err"),
        pair => {
          val (value,expectedValues) = pair
          value.asJson should be(expectedValues)
//          info(s"$values should be(${expectedValues})")
        }
      )
  }

  def processEntryNegative(name: String, shapePath: String, msgExpected: String): Unit = {
    val cmp = for {
      sp <- eitherStr2io(ShapePath.fromString(shapePath))
    } yield sp
    cmp.attempt
      .unsafeRunSync()
      .fold(
        err => info(s"Failed to parse as expected: $err"),
        value => fail(s"Parsed ${shapePath}as $value but should fail: $msgExpected")
      )
  }

  def matchNameIfSingle(nameIfSingle: Option[String], name: String): Boolean =
    nameIfSingle match {
      case None             => true
      case Some(nameSingle) => name == nameSingle
    }

  def processEntry(nameIfSingle: Option[String], all: Boolean)(entry: ManifestEntry): IO[Unit] =
    if (matchNameIfSingle(nameIfSingle, entry.name)) {
      val cmp: IO[Unit] = IO (entry.throws match {
        case None | Some(false) => processEntryPositive(entry.name, entry.from, entry.shapePath, entry.expect)
        case Some(true)         => processEntryNegative(entry.name, entry.shapePath, entry.expect)
      })
      for {
        _ <- IO {
          if (!all && entry.status != Some("accepted"))
            ignore(s"Entry: ${entry.name}") { }
          else {
            it(s"Should run entry: ${entry.name} shapePath: ${entry.shapePath}") { cmp.unsafeRunSync }
          }

        }
      } yield ()
    } else IO.pure(())

  def processManifest(manifest: Manifest,
                      nameIfSingle: Option[String] = None,
                      all: Boolean = false): IO[Unit] = {
    manifest.tests.map(processEntry(nameIfSingle, all)).sequence.void
  }

}
