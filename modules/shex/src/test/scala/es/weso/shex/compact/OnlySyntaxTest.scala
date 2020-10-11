package es.weso.shex.compact

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.utils.json.JsonTest
import es.weso.shex._
import es.weso.utils.FileUtils._
import org.scalatest.EitherValues
import scala.io._
import cats.effect._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class OnlySyntaxTest extends AnyFunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  val ignoreFiles = List("coverage")

  def getCompactFiles(schemasDir: String): IO[List[File]] = {
    getFilesFromFolderWithExt(schemasDir, "shex", ignoreFiles)
  }

  describe("Parsing Schemas from ShEx") {
    for (file <- getCompactFiles(schemasFolder).unsafeRunSync) {
      it(s"Should read Schema from file ${file.getName}") {
        val str = Source.fromFile(file)("UTF-8").mkString
        Schema.fromString(str).attempt.unsafeRunSync match {
          case Right(schema) => {
            val (name, ext) = splitExtension(file.getName)
            // TODO: Check that parsed file equals schema file
          }
          case Left(err) => fail(s"Parsing error: $err")
        }
      }
    }
  }
}
