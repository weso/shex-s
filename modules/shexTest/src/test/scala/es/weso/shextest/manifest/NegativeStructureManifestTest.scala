package es.weso.shextest.manifest

import java.nio.file.Paths
import com.typesafe.config.{Config, ConfigFactory}
import es.weso.shex._
// import scala.io._
import cats.data.EitherT
import cats.effect.IO


class NegativeStructureManifestTest extends ValidateManifest {

  // If the following variable is None, it runs all tests
  // Otherwise, it runs only the test whose name is equal to the value of this variable
  val nameIfSingle: Option[String] =
    None

  val ignored: Map[String,String] = Map(
    "TwoNegation2" -> "Shaclex only checks odd cycles as in the spec",
    "includeSimpleShape" -> "Non checked shape includes yet",
    "includeNonSimpleShape" -> "Non checked shape includes yet",
    "1focusRefANDSelfdot" -> "Not sure why it must fail",
    "1ShapeProductionCollision" -> "Collision between shape reference and tripleExpr reference, shaclex supports it, shouldn't it?",
    "includeExpressionNotFound" -> "Include checks not tested yet"
  )

  val conf: Config = ConfigFactory.load()
  val negativeStructureFolder = conf.getString("negativeStructureFolder")
  val folderUri = Paths.get(negativeStructureFolder).normalize.toUri
  // println(s"FolderURI=$folderUri")

  describe("RDF2ManifestLocal") {
    val r = RDF2Manifest.read(negativeStructureFolder + "/" + "manifest.ttl", "Turtle", Some(folderUri.toString), false)
    r.attempt.unsafeRunSync().fold(e => fail(s"Error reading manifest: $e"),
      mf => {
        for (e <- mf.entries) {
          if (nameIfSingle == None || nameIfSingle.getOrElse("") === e.name) {
            if (ignored.keySet.contains(e.name)) {
              ignore(s"${e.name} because: ${ignored.get(e.name).getOrElse("")}") {}
            } else
            it(s"Should test ${e.name}") {
              e match {
                case r: NegativeStructure => {
                  val fileName = Paths.get(r.shex.uri.getPath).getFileName.toString
                  val uri      = folderUri.resolve(fileName)
                  val res: IO[String] = for {
                    schemaStr <- derefUriIO(uri)
                    schema <- Schema.fromString(schemaStr, "SHEXC", None)
                    res <- schema.wellFormed match {
                        case Right(str) => ioErr(s"Schema is well formed, but should not\nSchema: $schema\nMsg: $str")
                        case Left(str) => IO(s"Schema is not well formed: $str\nSchema: ${schema}")
                    }
                  } yield res
                  res.attempt.unsafeRunSync.fold(s => fail(s"Error $s"), v => info(s"$v"))
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
