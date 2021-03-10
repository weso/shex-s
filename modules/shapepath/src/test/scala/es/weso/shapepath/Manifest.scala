package es.weso.shapepath
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.semiauto._

import es.weso.utils.testsuite._
import cats.implicits._
import cats.effect._
import java.nio.file.Path
import scala.io.{BufferedSource, Source}
import es.weso.shex.{IRILabel, Schema}
import es.weso.rdf.nodes.IRI
import cats._
import es.weso.utils.FileUtils


case class ManifestEntry(
    name: String,
    from: String,
    shexPath: String,
    expect: String,
    throws: Option[Boolean],
    status: Option[String],
    comment: Option[String]
) {
  def toTestEntry(manifestPath: String): TestEntry = TestEntry(name = TestId(name), action = 
    throws match {
        case None | Some(false) => processEntryPositive(name, from, shexPath, expect, manifestPath)
        case Some(true)         => processEntryNegative(name, shexPath, expect)
      }
   )

  def processEntryPositive(name: String, from: String, shapePath: String, expect: String, manifestPath: String): IO[Boolean] = {
    readContents2(manifestPath + from, manifestPath + expect + ".json").use(
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
        } yield (v.asJson === expectedValues)
      }
    )
  }

  def processEntryNegative(name: String, shapePath: String, msgExpected: String): IO[Boolean] = {
    ShapePath.fromString(shapePath).fold(
      err => IO.pure(true), 
      _ => IO.pure(false)
    )
  }

  private def readContents2(path1: String, path2: String): Resource[IO, (String,String)] =
    (readContents(path1), readContents(path2)).tupled

  private def readContents(path: String): Resource[IO, String] =
    readFile(path).map(_.mkString)

  private def readJsonContents(path: String): Resource[IO, Either[ParsingFailure, Json]] =
    readContents(path).map(parse(_))

  private def readFile(path: String): Resource[IO, BufferedSource] =
    Resource.fromAutoCloseable(IO(Source.fromFile(path)))

  private def either2io[E, A](e: Either[E, A], cnv: E => Throwable): IO[A] = {
    val either: Either[Throwable, A] = e.bimap(s => cnv(s), identity)
    MonadError[IO, Throwable].rethrow(IO(either))
  }

  private def eitherStr2io[A](e: Either[String, A]): IO[A] =
    either2io(e, cnvMsg)

  private def cnvMsg(msg: String): Throwable = new RuntimeException(msg)

  private def cnvFailure(df: ParsingFailure): Throwable = new RuntimeException(df.getMessage())

}

case class ManifestErrorParsingJson(failure: DecodingFailure) extends RuntimeException(s"Error obtaining manifest from json: ${failure.message}")
case class Manifest(
 description: String,
 tests: List[ManifestEntry]
) {
  def toTestSuite(manifestPath: String): TestSuite = TestSuite(
    tests.map(_.toTestEntry(manifestPath))
  )
}

object Manifest {

  implicit val entryDecoder: Decoder[ManifestEntry] = deriveDecoder
  implicit val entryEncoder: Encoder[ManifestEntry] = deriveEncoder
  implicit val manifestDecoder: Decoder[Manifest] = deriveDecoder
  implicit val manifestEncoder: Encoder[Manifest] = deriveEncoder

  def fromJson(json: Json): IO[Manifest] = json.as[Manifest].fold(
    f => IO.raiseError(ManifestErrorParsingJson(f)),
    IO.pure(_)
  )

  def fromString(str: String): IO[Manifest] = for {
    json <- IO.fromEither(parse(str).leftMap(df => new RuntimeException(s"JSON parsing error: ${df.message}")))
    _ <- IO.println(s"Json parsed: ${json}")
    mf <- fromJson(json)
  } yield mf

  def fromPath(path: Path): IO[Manifest] = for {
    str <- FileUtils.getContents(path)
    manifest <- fromString(str)
  } yield manifest
}

