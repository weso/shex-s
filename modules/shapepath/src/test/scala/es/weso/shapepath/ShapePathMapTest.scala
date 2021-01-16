package es.weso.shapepath
import cats.MonadError
import cats.effect.{IO, Resource}
import es.weso.rdf.nodes.IRI
// import cats.syntax.applicative._
import cats.implicits._
import scala.io.{BufferedSource, Source}
import munit._
import es.weso.shex.Schema

trait ShapePathMapTest extends FunSuite {

  val schemaStr = s"""|prefix foaf: <http://xmlns.com/foaf/0.1/>
                      |
                      |<Person> {
                      | foaf:firstName . ;
                      | foaf:lastName  .
                      }
                      |""".stripMargin

  val expectedStr = s"""|prefix schema: <http://schema.org/>
                        |<Person> {
                        | schema:givenName  . ;
                        | schema:familiName .
                        |}
                        |""".stripMargin
  val mappingsStr = s"""|prefix foaf: <http://xmlns.com/foaf/0.1/>
                        |prefix schema: <http://schema.org/>
                        |
                        |@<Person>/foaf:firstName ~> @<Person>/schema:givenName ;
                        |@<Person>/foaf:lastName ~> @<Person>/schema:familyName ;
                        |""".stripMargin

  val cmp: IO[Unit] = for {
    schema <- Schema.fromString(schemaStr)
    expected <- Schema.fromFile(expectedStr)
    mappings <- IO.fromEither(SchemaMappings.fromString(mappingsStr).leftMap(err => new RuntimeException(err.msg)))
    converted <- IO.fromEither(mappings.convert(schema).leftMap(err => new RuntimeException(err.toString)))
  } yield (converted,expected)

  cmp.attempt.unsafeRunSync().fold(
    err => fail(s"Error "),
    tuple => {
      val (converted,expected) = tuple
      assertEquals(converted,expected)
    }

  )

}
