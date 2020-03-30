package es.weso.shex.validator
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shapeMaps.ShapeMap
import es.weso.shex.Schema
import org.scalatest._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should._
import scala.util._
//import es.weso.utils.eitherios.EitherIOUtils._
import es.weso.shex.ResolvedSchema
import cats.data._ 
import cats.effect.IO

trait ShouldValidateShapeMap extends AnyFunSpecLike with Matchers {


  def shouldValidateWithShapeMap(
                                  rdfStr: String,
                                  shexStr: String,
                                  shapeMapStr: String,
                                  expected: String): Unit = {
    it(s"Should validate shapeMap: ${shapeMapStr} and return: $expected\nUsing RDF: \n ${rdfStr}\nand schema:\n${shexStr}") {

      def info(msg:String): EitherT[IO,String,Unit] = EitherT.liftF(IO(println(msg)))

      val validate: EitherT[IO,String,Boolean] = for {
        rdf <- EitherT.liftF(RDFAsJenaModel.fromChars(rdfStr, "Turtle",None))
        _ <- info(s"RDF: ${rdf}") 
        shex <- EitherT.liftF(Schema.fromString(shexStr, "ShExC", Some(IRI(""))))
        _ <- info(s"Schema: ${shex}") 
        shapeMap <- EitherT.fromEither[IO](ShapeMap.fromCompact(shapeMapStr, shex.base, rdf.getPrefixMap, shex.prefixMap))
        _ <- info(s"ShapeMap: ${shapeMap}") 
        fixedShapeMap <- EitherT.liftF(ShapeMap.fixShapeMap(shapeMap, rdf, rdf.getPrefixMap, shex.prefixMap))
        resolved <- EitherT.liftF(ResolvedSchema.resolve(shex,Some(IRI(""))))
        result <- EitherT.liftF(Validator.validate(resolved, fixedShapeMap, rdf))
        resultShapeMap <- EitherT.liftF(result.toResultShapeMap)
        expectedShapeMap <- EitherT.liftF(ShapeMap.parseResultMap(expected, shex.base, rdf, shex.prefixMap))
        _ <- info(s"Expected shapeMap parsed: $expectedShapeMap") 
        compare <- EitherT.fromEither[IO](resultShapeMap.compareWith(expectedShapeMap))
      } yield compare
      validate.value.unsafeRunSync match {
        case Left(msg) => fail(s"Error: $msg")
        case Right(v) => v should be(true)
      }
    }
  }



}