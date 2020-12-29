package es.weso.shextest.manifest

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.shex._
import cats.effect.IO
// import cats.data._
import org.scalatest._
import funspec.AnyFunSpec
// import matchers._

class NegativeSyntaxManifestTest extends AnyFunSpec with ValidateManifest {

  // If the following variable is None, it runs all tests
  // Otherwise, it runs only the test whose name is equal to the value of this variable
  val nameIfSingle: Option[String] =
    // Some("tripleConsraint-no-valueClass")
    None

  val ignored = Map(
    "directShapeExpression" -> "No file with that name in folder",
    "1iriLength2" -> "We allow several lengths by now",
    "1literalLength2" -> "We allow several lengths by now",
    "1unknowndatatypeMaxInclusive" -> "We allow MaxInclusive over unknown datatypes by now"
  )

  val conf: Config = ConfigFactory.load()
  val negativeSyntaxFolder = conf.getString("negativeSyntaxFolder")
  val folderUri = Paths.get(negativeSyntaxFolder).normalize.toUri
  // println(s"FolderURI=$folderUri")

  describe("RDF2ManifestLocal") {
    val r = RDF2Manifest.read(negativeSyntaxFolder + "/" + "manifest.ttl", "Turtle", Some(folderUri.toString), false)
    r.attempt.unsafeRunSync().fold(e => fail(s"Error reading manifest: $e"),
      mf => {
        for (e <- mf.entries) {
          if (nameIfSingle == None || nameIfSingle.getOrElse("") === e.name) {
            if (ignored.keySet.contains(e.name)) {
              ignore(s"${e.name} because: ${ignored.get(e.name).getOrElse("")}") {}
            } else
            it(s"Should test ${e.name}") {
              e match {
                case r: NegativeSyntax => {
                  val fileName = Paths.get(r.shex.uri.getPath).getFileName.toString
                  val uri      = folderUri.resolve(fileName)
                  val res : IO[String]= for {
                    schemaStr <- derefUriIO(uri)
                    e <- Schema.fromString(schemaStr, "SHEXC", None).attempt
                    v <- e.fold(
                     err => IO(s"Error as expected"),
                     schema => ioErr(s"ShEx parsed OK but should fail. String:\n${schemaStr}\nParsed as:\n${schema}"))
                  } yield v
                  res.attempt.unsafeRunSync.fold(fail(_), info(_))
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
