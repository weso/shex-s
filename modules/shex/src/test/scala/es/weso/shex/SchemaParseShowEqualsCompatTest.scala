package es.weso.shex

import java.io.File

import cats.implicits._
import cats.syntax.either._
import com.typesafe.config._
import es.weso.utils.json.JsonCompare.jsonDiff
import es.weso.utils.json._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._
import es.weso.utils.FileUtils._
import io.circe.syntax._
import org.scalatest._
import cats.data.EitherT
import cats.effect._

class SchemaParseShowEqualsCompatTest extends FunSpec with JsonTest with Matchers with EitherValues with OptionValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  val nameIfSingle =
    //"0focusIRI.shex".some
    none[String]

  val ignoreFiles = List(
    "coverage",
    "representationTests"
  )

  def getSchemaFiles(schemasDir: String): IO[List[File]] = {
    getFilesFromFolderWithExt(schemasDir, "shex", ignoreFiles)
  }

  describe("Parse Schema -> convert to Schema and parse again -> check both json representations") {
    for (file <- getSchemaFiles(schemasFolder).unsafeRunSync) {
      if (nameIfSingle == None || nameIfSingle.getOrElse("").equals(file.getName)) {
        it(s"Should process Schema from file ${file.getName}") {
          parseSchemaEncodeJsonEqualsJson(file)
        }
      }
    }
  }

  def parseSchemaEncodeJsonEqualsJson(file: File): Unit = {
    for {
      strSchema <- getContents(file)
      schema <- EitherT.liftF(Schema.fromString(strSchema)).leftMap((e: String) => s"Error obtainning Schema from string: $e\nString:\n${strSchema}")
      jsonEncoded = schema.asJson
      rdf <- EitherT.liftF(RDFAsJenaModel.empty)
      schemaShown <- EitherT.liftF(Schema.serialize(schema,"ShExC",None, rdf))
//      _ <- { println(s"SchemaShown: $schemaShown"); Right(()) }
      newSchemaParsed <- EitherT.liftF(Schema.fromString(schemaShown)).leftMap((e:String) =>
        s"Error parsing schema serialized: $e\nSchema serialized: \n$schemaShown\nOriginal schema:\n$strSchema\nInternal schema: ${schema}")
      jsonNew  = newSchemaParsed.asJson
      check <- if (jsonEncoded.equals(jsonNew)) EitherT.pure[IO,String](())
      else
        EitherT.leftT[IO,Unit](
          s"Jsons and different: Diff=${jsonDiff(jsonNew, jsonEncoded)}\nJson expected:\n${jsonNew.show}\nEncoded:\n${jsonEncoded.show}\nSchema:${schema.show}")
    } yield ()
  }.fold(e => fail(e), s => {})

}
