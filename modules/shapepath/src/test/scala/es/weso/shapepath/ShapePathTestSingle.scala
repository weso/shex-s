package es.weso.shapepath

import cats.MonadError
import cats.effect.{IO, Resource}
import es.weso.rdf.nodes.IRI
import es.weso.shex.{IRILabel, Schema}
import io.circe._
import io.circe.parser._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._
// import cats.syntax.applicative._
import cats.implicits._

import scala.io.{BufferedSource, Source}

class ShapePathTestSingle extends AnyFunSpec with ShapePathTest with  Matchers {

  describe(s"ShapePath from Manifest") {
      val cmp = readJsonContents(manifestPath + "Manifest.json").use(either =>
        either.fold(err => IO {
          println(s"Error parsing manifest: \n$err")
        }, json => for {
          manifest <- either2io(json2manifest(json), cnvMsg)
          _ <- processManifest(manifest, Some("nested_baseS0_p2_valueExpr_TC"))
        } yield ())
      )
      cmp.unsafeRunSync()
  }

}