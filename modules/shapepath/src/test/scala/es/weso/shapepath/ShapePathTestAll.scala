package es.weso.shapepath

import cats.MonadError
import cats.effect.{IO, Resource}
import es.weso.rdf.nodes.IRI
import es.weso.shex.{IRILabel, Schema, TripleConstraint}
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._
// import cats.syntax.applicative._
import cats.implicits._

import scala.io.{BufferedSource, Source}

class ShapePathTestAll extends  AnyFunSpec with ShapePathTest with Matchers {

  describe(s"JSON encoder") {
    it(s"Should encode value with triple expr") {
      val v: Value = Value(List(
        TripleExprItem(TripleConstraint.emptyPred(IRI("http://a.example/a")))
      ))
      val strJson = """|[
                       |      {
                       |        "type": "TripleConstraint",
                       |        "predicate": "http://a.example/a"
                       |      }
                       |]""".stripMargin
      val s = parse(strJson).fold(
        err => fail(s"Error parsing json: $err"),
        jsonExpected => v.asJson should be(jsonExpected)
      )
    }
  }

  describe(s"ShapePath") {
    it(s"Evaluates a shapePath") {
      // /@<#IssueShape>/2
      val two: TripleExprIndex = IntTripleExprIndex(2)
      val sTwo: Step = ExprStep(None, two)
      val issueShape: Step = ExprStep(None, ShapeLabelIndex(IRILabel(IRI("#IssueShape"))))
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

      val s: IO[Schema] = for {
        schema <- Schema.fromString(schemaStr)
      } yield (schema)

      s.attempt.unsafeRunSync match {
        case Left(e) => fail(s"Error: $e")
        case Right(s) => {
          val (es, v) = ShapePath.eval(path, s)
          es shouldBe empty
          info(s"Schema parsed:\n$s\nValue: $v")
        }
      }
    }
  }
  ignore(s"ShapePath from Manifest") {
      def runManifest(json: Json): IO[Unit] = for {
        manifest <- either2io(json2manifest(json), cnvMsg)
        _ <- processManifest(manifest, all = true)
      } yield ()

      val cmp = readJsonContents(manifestPath + "Manifest.json").use(either =>
        either.fold(err => IO {
          println(s"Error parsing manifest: \n$err")
        }, json => runManifest(json))
      )
      cmp.unsafeRunSync()
  }

  describe(s"Embedded manifest") {
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
        json <- either2io(parse(str), cnvFailure)
        manifest <- either2io(json2manifest(json), cnvMsg)
        _ <- processManifest(manifest)
      } yield ()
      cmp.attempt.unsafeRunSync.fold(e => println(s"Error: $e"), v => println(s"OK: $v"))
    }
}