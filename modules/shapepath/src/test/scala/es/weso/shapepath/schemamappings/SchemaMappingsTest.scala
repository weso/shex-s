package es.weso.shapepath.schemamappings
import cats.implicits._
import cats.effect._
import munit._
import es.weso.shex.Schema
// import es.weso.shex.implicits.eqShEx._
import io.circe.syntax._
import es.weso.shex.implicits.encoderShEx._

class SchemaMappingsTest extends CatsEffectSuite {

  test("Change foaf by schema") {

    val schemaStr = s"""|prefix foaf: <http://xmlns.com/foaf/0.1/>
                      |prefix xsd:  <http://www.w3.org/2001/XMLSchema#>
                      |prefix :     <http://example.org/>
                      |
                      |<Person> {
                      | foaf:firstName xsd:string ;
                      | foaf:lastName  xsd:string ;
                      | :code          xsd:integer
                      }
                      |""".stripMargin

    val expectedStr = s"""|prefix foaf:   <http://xmlns.com/foaf/0.1/>
                        |prefix schema: <http://schema.org/>
                        |prefix xsd:    <http://www.w3.org/2001/XMLSchema#>
                        |prefix :       <http://example.org/>
                        |
                        |<Person> {
                        | schema:givenName  xsd:string ;
                        | schema:familyName xsd:string ;
                        | :code             xsd:integer
                        |}
                        |""".stripMargin
    val mappingsStr = s"""|prefix foaf: <http://xmlns.com/foaf/0.1/>
                        |prefix schema: <http://schema.org/>
                        |
                        |@<Person>/foaf:firstName ~> schema:givenName ;
                        |@<Person>/foaf:lastName ~> schema:familyName
                        |""".stripMargin

    val cmp: IO[(Schema, Schema)] = for {
      schema <- Schema.fromString(schemaStr)
      expected <- Schema.fromString(expectedStr)
      // _ <- IO { println(s"Schemas read...")}
      mappings <- IO.fromEither(
        SchemaMappings.fromString(mappingsStr).leftMap(err => new RuntimeException(err.msg))
      )
      // _ <- IO { println(s"Mappings: ${mappings.toString}")}
      converted <- IO.fromEither(
        mappings.convert(schema).leftMap(err => new RuntimeException(err.toString)).toEither
      )
      // _ <- IO { println(s"Converted: \n${converted}\n")}
    } yield (converted, expected)

    cmp.attempt
      .unsafeRunSync()
      .fold(
        err => fail(s"Error ${err}"),
        tuple => {
          val (converted, expected) = tuple
          assertEquals(converted.asJson, expected.asJson)
        }
      )
  }

}
