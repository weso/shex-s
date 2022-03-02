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
import es.weso.shex._
import es.weso.rdf.nodes.IRI
import cats._
import es.weso.utils.FileUtils

import io.circe.CursorOp._

case class ManifestEntry(
    name: String,
    from: String,
    shapePath: String,
    expect: String,
    throws: Option[Boolean],
    status: Option[String],
    comment: Option[String]
) {
  def toTestEntry(manifestPath: String): TestEntry = {
    val id = TestId(name)
    val action: IO[TestResult] = throws match {
        case None | Some(false) => processEntryPositive(id, from, shapePath, expect, manifestPath)
        case Some(true)         => processEntryNegative(id, shapePath, expect)
      }
    TestEntry(id, action)
  }

  def processEntryPositive(id: TestId, from: String, shapePath: String, expect: String, manifestPath: String): IO[TestResult] = {
    readContents2(manifestPath + from, manifestPath + expect + ".json").use(
      pair => {
        val (schemaStr,expectStr) = pair
        for {
          schema <- Schema.fromString(schemaStr, "ShEXJ", Some(IRI("base:/")))
          shapePath <- eitherStr2io(ShapePath.fromString(shapePath, "Compact", Some(IRI("base:/"))))
          pair = ShapePath.eval(shapePath, schema)
          expectedValues <- either2io(parse(expectStr), cnvFailure)
          (es, v) = pair
          _ <- if (!es.isEmpty) IO.println(s"Processing errors: ${es.map(_.toString + "\n").mkString}")
               else IO.pure(())
        } yield 
        if (v.asJson === expectedValues) 
          PassedResult(id,msg = Some(s"JSon equals expected"))
        else 
          FailedResult(id,msg = Some(s"Json's are different =\n${v.asJson.spaces2}\nExpected: ${expectedValues.spaces2}"))  

      }
    )
  }

  def processEntryNegative(id: TestId, shapePath: String, msgExpected: String): IO[TestResult] = {
    ShapePath.fromString(shapePath).fold(
      err => IO.pure(PassedResult(id, msg = Some(s"Failed to parse as expected with msg: ${err}"))), 
      v => IO.pure(FailedResult(id,msg = Some(s"Parsed as $v but it was expected to fail\nshapePath: $shapePath")))
    )
  }

  private def readContents2(path1: String, path2: String): Resource[IO, (String,String)] =
    (readContents(path1), readContents(path2)).tupled

  private def readContents(path: String): Resource[IO, String] =
    readFile(path).map(_.mkString)

/*  private def readJsonContents(path: String): Resource[IO, Either[ParsingFailure, Json]] =
    readContents(path).map(parse(_)) */

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

case class ManifestErrorParsingJson(failure: DecodingFailure, line: String, json: String) extends 
  RuntimeException(s"Error obtaining manifest from json: ${failure.message}\nLine: ${line}\nJson:\n ${json}")
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

  // The following code has been borrowed from: https://github.com/circe/circe/issues/1464
  def historyToLineNumber(cursor: ACursor, history: Seq[CursorOp]): String = {
  history.headOption match {
    // support other CursorOp subclasses...
    case Some(DownField(f)) => historyToLineNumber(cursor.downField(f), history.tail)
    case _ =>
      val target = "<Intentionally_Break_The_Json_Parser_HERE_To_Get_Line_Number!>"
      val broken = parse(
        cursor
        .withFocus(_ => Json.fromString(target))
        .top.get.toString.replace(s""""$target"""", ">something that will break it!")
      )
      broken.toString // It shouldn't arrive here
  }
}

//val errorMessageContainingLineNumber = historyToLineNumber(hcursor.downField("Top"), history.reverse)

  def fromJson(json: Json): IO[Manifest] = json.as[Manifest].fold(
    f => { 
      val line = historyToLineNumber(json.hcursor, f.history.reverse)
      IO.raiseError(ManifestErrorParsingJson(f, line, json.spaces2))
    },
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

