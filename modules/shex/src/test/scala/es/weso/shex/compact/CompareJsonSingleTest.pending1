package es.weso.shex.compact

import java.io.File

import cats.implicits._
import com.typesafe.config.{Config, ConfigFactory}
import es.weso.shex._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._
import es.weso.utils.FileUtils._
import es.weso.utils.json.{JsonCompare, JsonTest}
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.EitherValues

import scala.io._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CompareJsonSingleTest extends AnyFunSpec with JsonTest with Matchers with EitherValues {

  val name = "1val1emptylanguageStemMinuslanguage3"
  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  private def splitExtension(str: String): (String, String) = {
    val splits = str.split('.')
    (splits.init.mkString("."), splits.last)
  }

  describe(s"Parsing single File $name") {
    val file: File = getFileFromFolderWithExt(schemasFolder, name, "shex").unsafeRunSync()
    it(s"Should read Schema from file ${file.getName}") {
      val str = Source.fromFile(file)("UTF-8").mkString
      Schema.fromString(str).attempt.unsafeRunSync match {
        case Right(schema) => {
          val (name, ext) = splitExtension(file.getName)
          val jsonFile = schemasFolder + "/" + name + ".json"
          val jsonStr = Source.fromFile(jsonFile)("UTF-8").mkString
          parse(jsonStr) match {
            case Left(err) => fail(s"Error parsing $jsonFile: $err")
            case Right(json) => {
              val json2 = schema.asJson
              if (json.equals(json2)) {
                info("Jsons are equal")
              } else {
                fail(s"""|Json's are different. Parsed:\n${schema.asJson.spaces4}
                        |Expected:${json.spaces4}
                        |Diff: ${JsonCompare.diffBasic(json,json2)}
                        |Diffson: ${JsonCompare.jsonDiff(json,json2)}
                        |Schema\n${schema.show}
                        |Plain schema\n${schema}""".stripMargin)
              }
            }
          }
        }
        case Left(err) => fail(s"Parsing error: $err")
      }
    }
  }
}
