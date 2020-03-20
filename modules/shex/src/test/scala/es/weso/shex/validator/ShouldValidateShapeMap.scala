package es.weso.shex.validator
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shapeMaps.ShapeMap
import es.weso.shex.Schema
import org.scalatest._
import scala.util._
import es.weso.utils.eitherios.EitherIOUtils._
import es.weso.shex.ResolvedSchema

trait ShouldValidateShapeMap extends FunSpecLike with Matchers {


  def shouldValidateWithShapeMap(
                                  rdfStr: String,
                                  shexStr: String,
                                  shapeMapStr: String,
                                  expected: String): Unit = {
    it(s"Should validate shapeMap: ${shapeMapStr} and return: $expected\nUsing RDF: \n ${rdfStr}\nand schema:\n${shexStr}") {
      val validate = for {
        rdf <- RDFAsJenaModel.fromChars(rdfStr, "Turtle",None)
        shex <- Schema.fromString(shexStr, "ShExC", Some(IRI("")))
        shapeMap <- {
          eitherStr2IO(ShapeMap.fromCompact(shapeMapStr, shex.base, rdf.getPrefixMap, shex.prefixMap))
        }
        fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap, rdf, rdf.getPrefixMap, shex.prefixMap)
        resolved <- ResolvedSchema.resolve(shex,Some(IRI("")))
        result <- Validator.validate(resolved, fixedShapeMap, rdf)
        resultShapeMap <- result.toResultShapeMap
        expectedShapeMap <- ShapeMap.parseResultMap(expected, shex.base, rdf, shex.prefixMap)
        // _ <- { info(s"Expected shapeMap parsed: $expectedShapeMap"); Right(())}
        compare <- eitherStr2IO(resultShapeMap.compareWith(expectedShapeMap))
      } yield compare
      validate.attempt.unsafeRunSync match {
        case Left(msg) => fail(s"Error: $msg")
        case Right(v) => v should be(true)
      }
    }
  }



}