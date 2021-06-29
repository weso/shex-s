package es.weso.shex.compact

import java.io.File
import cats.effect._
import cats.data.EitherT
import com.typesafe.config.{Config, ConfigFactory}
import es.weso.utils.json.JsonTest
import es.weso.utils.FileUtils._
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.Paths

class ParseSchemaFileSingleTest extends AnyFunSpec with JsonTest with Matchers with EitherValues {

  val name = "1focusLength-dot"
  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  describe(s"Parsing single File $name") {
    it(s"Should read Schema from file ${name}") {
      val parsed = for {
        file <- EitherT.liftF[IO,String,File](getFileFromFolderWithExt(schemasFolder, name, "shex"))
        cs <- EitherT.liftF[IO,String,String](getContents(Paths.get(file.getAbsolutePath())))
        schema <- EitherT.fromEither[IO](Parser.parseSchema(cs.toString,None))
      } yield schema
      parsed.value.unsafeRunSync match {
        case Right(schema) => info(s"Parsed:\n$schema")
        case Left(err) => fail(s"Parsing error: $err")
      }
    }
  }

}
