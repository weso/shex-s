package es.weso.shex
import org.scalatest._
import com.typesafe.config._
import java.io.File
import scala.io._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._
import es.weso.utils.json._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ShExJsonSingleTest extends AnyFunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  def getJsonFile(schemasDir: String, name: String): File = {
    getFileFromFolderWithExt(schemasDir, name, "json")
  }

  def getFileFromFolderWithExt(path: String, name: String, ext: String): File = {
    new File(schemasFolder + "/" + name + ".json")
  }

  describe("Parsing Schema from Json") {
    val name = "ExtendAND3G"
    val file = getJsonFile(schemasFolder, name)
    it(s"Should read Schema from file ${file.getName}") {
      val str = Source.fromFile(file)("UTF-8").mkString
      decodeJsonSchemaEncodeEquals[Schema](str)
    }
  }
}