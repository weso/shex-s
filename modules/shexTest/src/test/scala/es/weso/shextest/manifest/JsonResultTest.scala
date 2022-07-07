package es.weso.shextest.manifest

import java.nio.file.Paths
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.parser._
import io.circe.syntax._
import munit._
import scala.io.Source

class JsonResultTest extends FunSuite {

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("validationFolder")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri

  test(s"ShapeResult parsing") {
    val jsonStr = "{\"shape\": \"http://schema.example/IssueShape\", \"result\": true}"
    decode[ShapeResult](jsonStr).fold(
      e => fail(s"Error parsing: $e"),
      result => assertEquals(result.value, true)
    )
  }

  {
    val name = "node_kind_example_results.json"
    test(s"Should parse ResultParsing $name") {
      val jsonStr = Source.fromURI(shexFolderURI.resolve(name)).mkString
      decode[JsonResult](jsonStr)
        .fold(e => fail(s"Error: $e"), result => assertEquals(result.rmap.toList.size > 0, true))
    }
  }
}
