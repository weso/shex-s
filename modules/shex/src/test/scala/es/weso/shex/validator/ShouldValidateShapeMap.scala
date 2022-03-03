package es.weso.shex.validator


import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shapemaps.ShapeMap
import es.weso.shex.Schema
// import org.scalatest._

import scala.util._
import cats.implicits._
import cats.effect._
import es.weso.shex.ResolvedSchema
import es.weso.shex.implicits.encoderShEx._
// import io.circe._
import io.circe.syntax._
import es.weso.utils.IOUtils.fromES
import munit._
import es.weso.utils.VerboseLevel

trait ShouldValidateShapeMap extends CatsEffectSuite {


  def shouldValidateWithShapeMap(rdfStr: String,
                                 shexStr: String,
                                 shapeMapStr: String,
                                 expected: String,
                                 verbose: VerboseLevel = VerboseLevel.Info)
                                 (implicit loc: munit.Location): Unit = {
    test(s"Should validate shapeMap: ${shapeMapStr} and return: $expected\nUsing RDF: \n ${rdfStr}\nand schema:\n${shexStr}") {

      def info(msg:String): IO[Unit] =
        verbose.info(msg)

      
      val validate: IO[Boolean] = for {
        _ <- info(s"Before all validate...")  
        res1 <- RDFAsJenaModel.fromString(rdfStr, "Turtle",None)
        res2 <- RDFAsJenaModel.empty
        vv <- (res1,res2).tupled.use{ 
        case (rdf,builder) =>
         for {
          shex <- Schema.fromString(shexStr, "ShExC", Some(IRI("")))
          rdfPrefixMap <- rdf.getPrefixMap
          shapeMap <- fromES(ShapeMap.fromCompact(shapeMapStr, shex.base, rdfPrefixMap, shex.prefixMap).leftMap(_.toList.mkString("\n")))
          fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap, rdf, rdfPrefixMap, shex.prefixMap)
          resolved <- ResolvedSchema.resolve(shex,Some(IRI("")), verbose)
          result <- Validator.validate(resolved, fixedShapeMap, rdf, builder,verbose)
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
      assertIO(validate, true)
    }
  }

}