package es.weso.shex
import org.scalatest._
import com.typesafe.config._
import java.io.File
import scala.io._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._
import es.weso.utils.json._
import es.weso.utils.FileUtils._
<<<<<<< HEAD
import cats.effect._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

=======
import cats.effect._
import matchers.should._
import funspec._


>>>>>>> issue57
class ShExJsonTest extends AnyFunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  val ignoreFiles = List("coverage", "representationTests")

  def getJsonFiles(schemasDir: String): IO[List[File]] = {
    getFilesFromFolderWithExt(schemasDir, "json", ignoreFiles)
  }

  describe("Parsing Schemas from Json") {
    for (file <- getJsonFiles(schemasFolder).unsafeRunSync()) {
      it(s"Should read Schema from file ${file.getName}") {
        val str = Source.fromFile(file)("UTF-8").mkString
        decodeJsonSchemaEncodeEquals[Schema](str)
      }
    }
  }

}
