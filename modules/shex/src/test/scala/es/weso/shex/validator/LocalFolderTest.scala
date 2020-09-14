package es.weso.shex.validator

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.utils.FileUtils.getFilesFromFolderWithExt
import cats.effect._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class LocalFolderTest extends AnyFunSpec with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load
  val schemasFolder = conf.getString("localFolderTest")

  val ignoreFiles = List("coverage")

  def getJsonFiles(schemasDir: String): IO[List[File]] = {
    getFilesFromFolderWithExt(schemasDir, "json", ignoreFiles)
  }

  describe("Local folder test") {

  }
}
