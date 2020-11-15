package es.weso.shextest.manifest

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.shex._
import es.weso.shex.compact.CompareSchemas
import io.circe.parser._
import io.circe.syntax._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import Utils._
import es.weso.utils.IOUtils.fromES
// import es.weso.utils.UriUtils._
//import cats._
// import cats.data._
import cats.effect.IO
import cats.implicits._

class SchemasManifestTest extends ValidateManifest {

  val nameIfSingle: Option[String] =
     None
     // Some("1dotAbstractShapeCode1")

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("schemasFolder")
//  val shexFolder = conf.getString("shexLocalFolder")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri

  describe("RDF2ManifestLocal") {
    val r = RDF2Manifest.read(shexFolder + "/" + "manifest.ttl", "Turtle", Some(shexFolderURI.toString), false)
    r.attempt.unsafeRunSync().fold(e => fail(s"Error reading manifest: $e"),
      mf => {
        for (e <- mf.entries) {
          if (nameIfSingle == None || nameIfSingle.getOrElse("") == e.name) {
            it(s"Should pass test ${e.name}") {
              println(s"Testing: ${e.name}")
              e match {
                case r: RepresentationTest => {
                  val schemaUri = mkLocal(r.shex, schemasBase, shexFolderURI)
                  val jsonUri   = mkLocal(r.json, schemasBase, shexFolderURI)
                  val either: IO[String] = for {
                    schemaStr      <- derefUriIO(schemaUri)
                    jsonStr        <- derefUriIO(jsonUri)
                    schema         <- Schema.fromString(schemaStr, "SHEXC", None)
                    _ <- IO { println(s"Schema: ${schema}") }
                    _ <- IO { println(s"Checking if it is well formed...") }
                    // b              <- schema.wellFormed
                    // _ <- { println(s"Schema well formed?: ${b.toString}"); Right(()) }

                    expectedSchema <- fromES(decode[Schema](jsonStr).leftMap(_.getMessage()))
                    _ <- IO { println(s"Expected schema: ${expectedSchema}") }
                    _ <- if (CompareSchemas.compareSchemas(schema, expectedSchema)) 
                           IO.pure("Schemas are equal")
                         else 
                           IO { pprint.log(schema,"Parsed schema")} *>
                           IO { pprint.log(expectedSchema, "Expected schema") } *>
                           ioErr(s"""|Schemas are different. 
                                     |Parsed:
                                     |${schema}
                                     |Expected:
                                     |${expectedSchema}
                                     |""".stripMargin)
                    json <- fromES(parse(jsonStr).leftMap(_.message))
                    check <- if (json.equals(schema.asJson)) 
                                 IO.pure(s"Schemas are equal")
                             else 
                               ioErr(s"Json's are different\nSchema:${schema}\nJson generated: ${schema.asJson.spaces2}\nExpected: ${json.spaces2}")
                  } yield check
                  either.attempt.unsafeRunSync().fold(
                    e => fail(s"Error: $e"),
                    msg => info(msg)
                  )
                }
              }
            }
          }
        }
        info(s"Manifest read OK: ${mf.entries.length} entries")
      }
    )
   }

  private def ioErr[A](msg: String): IO[A] = IO.raiseError(new RuntimeException(msg))

}
