package es.weso.shex.validator
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shapeMaps.ShapeMap
import es.weso.shex.Schema
// import org.scalatest._

import scala.util._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
// import cats.data._
import cats.effect._
import es.weso.shex.ResolvedSchema
import es.weso.utils.IOUtils.fromES


trait ShouldValidateShapeMap extends AnyFunSpecLike with Matchers {


  def shouldValidateWithShapeMap(
                                  rdfStr: String,
                                  shexStr: String,
                                  shapeMapStr: String,
                                  expected: String): Unit = {
    it(s"Should validate shapeMap: ${shapeMapStr} and return: $expected\nUsing RDF: \n ${rdfStr}\nand schema:\n${shexStr}") {

      def info(msg:String): IO[Unit] =
        // IO(println(msg)
        IO.pure(())

      val validate: IO[Boolean] = RDFAsJenaModel.fromChars(rdfStr, "Turtle",None).use(rdf =>
        for {
        _ <- info(s"RDF: ${rdf}")
        shex <- Schema.fromString(shexStr, "ShExC", Some(IRI("")))
        _ <- info(s"Schema: ${shex}")
        rdfPrefixMap <- rdf.getPrefixMap
        shapeMap <- fromES(ShapeMap.fromCompact(shapeMapStr, shex.base, rdfPrefixMap, shex.prefixMap))
        _ <- info(s"ShapeMap: ${shapeMap}") 
        fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap, rdf, rdfPrefixMap, shex.prefixMap)
        resolved <- ResolvedSchema.resolve(shex,Some(IRI("")))
        result <- Validator.validate(resolved, fixedShapeMap, rdf)
        resultShapeMap <- result.toResultShapeMap
        expectedShapeMap <- ShapeMap.parseResultMap(expected, shex.base, rdf, shex.prefixMap)
        _ <- info(s"Expected shapeMap parsed: $expectedShapeMap") 
        compare <- fromES(resultShapeMap.compareWith(expectedShapeMap))
      } yield compare)
      validate.attempt.unsafeRunSync match {
        case Left(msg) => fail(s"Error: $msg")
        case Right(v) => v should be(true)
      }
    }
  }

}