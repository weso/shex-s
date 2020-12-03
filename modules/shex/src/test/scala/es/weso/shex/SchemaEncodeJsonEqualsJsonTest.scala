package es.weso.shex

import java.io.File

import cats.implicits._
import cats.syntax.either._
import com.typesafe.config._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._
import es.weso.utils.FileUtils._
import es.weso.utils.json.JsonCompare.jsonDiff
import es.weso.utils.json._
import io.circe.parser.parse
import io.circe.syntax._
import cats.data.EitherT
import org.scalatest._
import cats.effect._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.Paths

class SchemaEncodeJsonEqualsJsonTest extends AnyFunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  val ignoreFiles = List(
    "coverage",
    "representationTests",
    "open1dotclose",
    "open1dotclosecardOpt",
    "open1dotcloseCode1",
    "openopen1dotcloseCode1closeCode3",
    "openopen1dotOr1dotclose"
  )

  def getSchemaFiles(schemasDir: String): IO[List[File]] = {
    getFilesFromFolderWithExt(schemasDir, "shex", ignoreFiles)
  }

  describe("Parsing Schemas from Json") {
    for (file <- getSchemaFiles(schemasFolder).unsafeRunSync) {
      it(s"Should read Schema from file ${file.getName}") {
        parseSchemaEncodeJsonEqualsJson(file)
      }
    }
  }

  def parseSchemaEncodeJsonEqualsJson(file: File): Unit = {
    for {
      strSchema <- EitherT.liftF(getContents(Paths.get(file.getAbsolutePath())))
      fileJson <- EitherT.liftF(getFileFromFolderWithSameExt(file,".shex",".json"))
      strJson <- EitherT.liftF(getContents(Paths.get(fileJson.getAbsolutePath())))
      jsonExpected <- EitherT.fromEither[IO](parse(strJson.toString).leftMap(e => s"Error parsing $strJson: $e"))
      schema <- EitherT.liftF(Schema.fromString(strSchema)).leftMap((e: String) => s"Error obtainning Schema from string: $e\nString:\n${strSchema}")
      jsonEncoded = schema.asJson
      check <- if (jsonEncoded.equals(jsonExpected)) EitherT.pure[IO,String](())
      else
      EitherT.leftT[IO,Unit](
                  s"Jsons and different: Diff=${jsonDiff(jsonExpected, jsonEncoded)}\nJson expected:\n${jsonExpected.show}\nEncoded:\n${jsonEncoded.show}\nSchema:${schema.show}")
    } yield check
  }.fold(e => fail(e), s => {})

}
