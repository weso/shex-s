package es.weso.shapepath

import es.weso.rdf.nodes.IRI
import es.weso.shex.{IRILabel, Schema, TripleConstraint}
import io.circe.syntax._
import io.circe.parser._
// import cats.implicits._
import cats.effect._
// import cats.syntax.applicative._
import munit._
import es.weso.utils.testsuite._
import java.nio.file.Paths

class ShapePathTestAll extends CatsEffectSuite {
  val manifestPath = "modules/shapepath/src/test/resources/test-suite/"

  test(s"Should encode value with triple expr") {
      val v: Value = Value(List(
        TripleExprItem(TripleConstraint.emptyPred(IRI("http://a.example/a")))
      ))
      val strJson = """|[
                       |      {
                       |        "type": "TripleConstraint",
                       |        "predicate": "http://a.example/a"
                       |      }
                       |]""".stripMargin
      parse(strJson).fold(
        err => fail(s"Error parsing json: $err"),
        jsonExpected => assertEquals(v.asJson, jsonExpected)
      )
  }

  test(s"Evaluates a shapePath") {
      // /@<#IssueShape>/2
      // val two: TripleExprIndex = IntTripleExprIndex(2)
      // val sTwo: Step = ExprStep(None, two, List())
      val issueShape: Step = ExprStep(None, ShapeLabelIndex(IRILabel(IRI("#IssueShape"))), List())
      val path: ShapePath = ShapePath(true, List(issueShape))

      val schemaStr =
        s"""|prefix : <http://example.org/>
            |prefix foaf: <http://xmlns.com/foaf/0.1/>
            |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
            |
            |<#IssueShape> {
            |  :name xsd:string MinLength 4;
            |  :category ["bug" "feature request"];
            |  :postedBy @<#UserShape>;
            |  :processing {
            |    :reproduced [true false];
            |    :priority xsd:integer
            |   }?
            | }
            |
            |<#UserShape> IRI /User\\?id=[0-9]+/ {
            | ( foaf:name xsd:string
            | | foaf:givenName . + ;
            |   foaf:familyName . ) ;
            | foaf:mbox IRI
            |}
            |""".stripMargin

      val cmp: IO[Schema] = for {
        schema <- Schema.fromString(schemaStr)
      } yield (schema)

      cmp.map(s => {
          val (es, v) = ShapePath.eval(path, s)
          assertEquals(es.size,0)
          // info(s"Schema parsed:\n$s\nValue: $v")
        }
      )
  }

  test(s"ShapePath from Manifest".ignore) {
      val cmp = for { 
        manifest <- Manifest.fromPath(Paths.get(manifestPath + "Manifest.json")) 
        testSuite = manifest.toTestSuite(manifestPath)
        pair <- testSuite.runAll(TestConfig.initial)
      } yield pair
      cmp.map { case (_, failed) => assertEquals(failed.map(_.entry.name.id), Vector[String]()) }
  } 

  test(s"Embedded manifest".ignore) {
    val str =
        """|{
           |  "description": "collection of partition tests",
           |  "tests": [
           |    {
           |      "name": "2Eachdot_S_a",
           |      "from": "./2Eachdot.json",
           |      "shexPath": "/@<http://a.example/S>/<http://a.example/a>",
           |      "expect": "2Eachdot_S_a"
           |    },
           |    {
           |      "name": "nested_s0_2_1*",
           |      "from": "./nested.json",
           |      "shexPath": "@<base:/S0>/2/1*",
           |      "throws": true,
           |      "expect": "Error: unable to parse at offset 15: *"
           |    }
           | ]
           |}""".stripMargin


      val cmp = for {
        manifest <- Manifest.fromString(str)
        testSuite = manifest.toTestSuite(manifestPath)
        pair <- testSuite.runAll(TestConfig.initial)
      } yield pair
      cmp.map { case (_, failed) => assertEquals(failed.map(_.entry.name.id), Vector[String]()) }
    } 

}