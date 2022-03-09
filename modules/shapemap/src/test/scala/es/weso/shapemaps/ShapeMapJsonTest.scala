package es.weso.shapemaps

import es.weso.rdf._
import es.weso.rdf.nodes._
import munit._

class ShapeMapJsonTest extends FunSuite {

  test("can parse Json example") {
    val str =
      """
          |[
          |  {"node":"http://inst.example/Issue1", "shape":"http://schema.example/IssueShape"},
          |  {"node":"http://inst.example/Issue2", "shape":"http://schema.example/IssueShape"},
          |  {"node":"http://inst.example/Issue3", "shape":"http://schema.example/IssueShape"}
          |]
        """.stripMargin
    ShapeMap
      .fromJson(str)
      .fold(
        e => fail(s"Error parsing: $e"),
        sm => { () }
      )
  }

  val npm = PrefixMap.empty.addPrefix("", IRI("http://example.org/"))
  val spm = npm
  encodeDecodeJson("<http://example.org/user>@<http://example.org/User>", npm, spm)
  encodeDecodeJson("{ _ a FOCUS }@:User", npm, spm)

  def encodeDecodeJson(str: String, nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): Unit = {
    test(s"Should encodeDecode as Json $str") {
      val result = for {
        shapeMap <- ShapeMap.fromCompact(str, None, nodesPrefixMap, shapesPrefixMap)
        json = {
          // println(s"ShapeMap: $shapeMap")
          shapeMap.toJson
        }
        shapeMapFromJson <- {
          // println(s"Json: ${json.spaces2}")
          ShapeMap.fromJson(json.spaces2)
        }
      } yield (json, shapeMapFromJson.toJson)
      result match {
        case Left(str) => fail(s"Error encoding/decoding Json: $str")
        case Right((json1, json2)) =>
          assertEquals(json1.spaces2, json2.spaces2)
      }
    }

  }
}
