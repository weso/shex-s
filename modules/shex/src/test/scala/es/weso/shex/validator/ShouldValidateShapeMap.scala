package es.weso.shex.validator


import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shapeMaps.ShapeMap
import es.weso.shex.Schema
// import org.scalatest._

import scala.util._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import cats.implicits._
import cats.effect._
import es.weso.shex.ResolvedSchema
import es.weso.shex.implicits.encoderShEx._
// import io.circe._
import io.circe.syntax._
import es.weso.utils.IOUtils.fromES


trait ShouldValidateShapeMap extends AnyFunSpecLike with Matchers {


  def shouldValidateWithShapeMap(rdfStr: String,
                                 shexStr: String,
                                 shapeMapStr: String,
                                 expected: String,
                                 verbose: Boolean = false): Unit = {
    it(s"Should validate shapeMap: ${shapeMapStr} and return: $expected\nUsing RDF: \n ${rdfStr}\nand schema:\n${shexStr}") {

      def info(msg:String): IO[Unit] =
        if (verbose) IO(println(msg))
        else IO.pure(())

      
      val validate: IO[Boolean] = for {
        _ <- info(s"Before all validate...")  
        res1 <- RDFAsJenaModel.fromString(rdfStr, "Turtle",None)
        _ <- info(s"Res 1: ${res1}")
        res2 <- RDFAsJenaModel.empty
        _ <- info(s"Res 2: ${res2}")
        vv <- (res1,res2).tupled.use{ 
        case (rdf,builder) =>
         for {
          _ <- info("Before validating...") 
          _ <- info(s"RDF: ${rdf}")
          shex <- Schema.fromString(shexStr, "ShExC", Some(IRI("")))
          _ <- info(s"Schema: ${shex.asJson}")
          rdfPrefixMap <- rdf.getPrefixMap
          shapeMap <- fromES(ShapeMap.fromCompact(shapeMapStr, shex.base, rdfPrefixMap, shex.prefixMap))
          _ <- info(s"ShapeMap: ${shapeMap}") 
          fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap, rdf, rdfPrefixMap, shex.prefixMap)
          _ <- info(s"Fixed shapeMap: ${fixedShapeMap}") 
          resolved <- ResolvedSchema.resolve(shex,Some(IRI("")))
          _ <- info(s"ResolvedSchema: ${resolved}") 
          result <- Validator.validate(resolved, fixedShapeMap, rdf, builder,verbose)
          _ <- info(s"Result: ${result}") 
          resultShapeMap <- result.toResultShapeMap
          _ <- info(s"Result shapeMap: ${resultShapeMap}") 
          expectedShapeMap <- ShapeMap.parseResultMap(expected, shex.base, rdf, shex.prefixMap)
          _ <- info(s"Expected shapeMap parsed: $expectedShapeMap") 
          compare <- resultShapeMap.compareWith(expectedShapeMap) match {
            case Left(str) => 
               info(str) *> 
               IO(false)
            case Right(b) => IO(b)
          }
          _ <- info(s"Value of compared: $compare")
        } yield compare 
       }
      } yield vv
      validate.attempt.unsafeRunSync match {
        case Left(msg) => fail(s"Error: $msg")
        case Right(v) => v should be(true)
      }
    }
  }

}