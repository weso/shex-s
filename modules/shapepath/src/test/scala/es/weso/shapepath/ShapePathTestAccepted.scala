package es.weso.shapepath

import cats.effect.IO
import es.weso.rdf.nodes.IRI
import es.weso.shex.{IRILabel, Schema, TripleConstraint}
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._
// import cats.syntax.applicative._

class ShapePathTestAccepted extends  AnyFunSpec with ShapePathTest with Matchers {

  describe(s"ShapePath from Manifest only accepted tests") {
    def runManifest(json: Json): IO[Unit] = for {
      manifest <- either2io(json2manifest(json), cnvMsg)
      _ <- processManifest(manifest, all = false)
    } yield ()

    val cmp = readJsonContents(manifestPath + "Manifest.json").use(either =>
      either.fold(err => IO {
        println(s"Error parsing manifest: \n$err")
      }, json => runManifest(json))
    )
    cmp.unsafeRunSync()
  }

}