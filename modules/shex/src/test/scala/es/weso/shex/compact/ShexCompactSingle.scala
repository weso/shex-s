package es.weso.shex.compact
import org.scalatest._
import com.typesafe.config._
import java.io.File

import scala.io._
import es.weso.utils.json._
import es.weso.utils.FileUtils._
import es.weso.shex._
import cats.effect._
<<<<<<< HEAD
import cats.implicits._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
=======
import cats.implicits._
import matchers.should._
import funspec._
>>>>>>> issue57

class ShexCompactSingle extends AnyFunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  val ignoreFiles = List("coverage")

  val files: List[String] =
    List("1val1vExprRefOR3")

  def getCompactFiles(schemasDir: String): IO[List[File]] = {
    files.map(getFileFromFolderWithExt(schemasDir, _, "shex")).sequence
  }

  describe("Parsing Schemas from ShEx") {
    for (file <- getCompactFiles(schemasFolder).unsafeRunSync()) {
      it(s"Should read Schema from file ${file.getName}") {
        val str = Source.fromFile(file)("UTF-8").mkString
        Schema.fromString(str, "SHEXC", None).attempt.unsafeRunSync match {
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
