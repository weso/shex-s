package es.weso.shex.compact

import java.io.File
import com.typesafe.config.{Config, ConfigFactory}
import es.weso.utils.json.JsonTest
import es.weso.shex._
import es.weso.shex.implicits.decoderShEx._
import es.weso.utils.FileUtils._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.EitherValues
import es.weso.shex.implicits.encoderShEx._
import cats.data.EitherT
import cats.effect._
import scala.io._
import cats.implicits._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CompareSchemasSingleTest extends AnyFunSpec with JsonTest with Matchers with EitherValues {

  val name          = "1val1emptylanguageStem"
  val conf: Config  = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  describe(s"Parsing single File $name") {
    it(s"Should read Schema from file ${name}") {

      val either = for {
        file <- EitherT.liftF[IO,String,File](getFileFromFolderWithExt(schemasFolder, name, "shex"))
        str <- EitherT(IO(Source.fromFile(file)("UTF-8").mkString.asRight[String]))
        schema <- EitherT.fromEither[IO](Schema.fromString(str))
      } yield (schema,file)
      either.value.unsafeRunSync match {
        case Right(pair) => {
          val (schema,file) = pair
          val (name, ext) = splitExtension(file.getName)
          val jsonFile    = schemasFolder + "/" + name + ".json"
          val jsonStr     = Source.fromFile(jsonFile)("UTF-8").mkString
          decode[Schema](jsonStr) match {
            case Left(err) => fail(s"Error parsing $jsonFile: $err")
            case Right(expectedSchema) =>
              if (CompareSchemas.compareSchemas(schema, expectedSchema)) {
                info("Schemas are equal")
              } else {
                fail(
                  s"Schemas are different. Parsed:\n${schema}\n-----Expected:\n${expectedSchema}\nParsed as Json:\n${schema.asJson.spaces2}\nExpected as Json:\n${expectedSchema.asJson.spaces2}")
              }
          }          
        }
        case Left(err) => fail(s"Parsing error: $err")
      }
    }
  }


}
