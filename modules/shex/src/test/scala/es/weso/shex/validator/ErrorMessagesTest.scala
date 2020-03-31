package es.weso.shex.validator

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.shapeMaps.{ShapeMap, ShapeMapLabel, IRILabel}
import es.weso.shex.Schema
<<<<<<< HEAD
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

=======
import org.scalatest._
import es.weso.utils.eitherios.EitherIOUtils._
import es.weso.shex.ResolvedSchema
import matchers.should._
import funspec._


>>>>>>> issue57
class ErrorMessagesTest extends AnyFunSpec with Matchers with EitherValues {

  describe("Error messages test") {
    it(s"Should generate good error message") {
      val rdfStr =
        s"""prefix : <http://example.org/>
           |:x :p "One" .
         """.stripMargin
      val shexStr =
        s"""prefix : <http://example.org/>
           |:S { :p BNode | :p  IRI }
         """.stripMargin
      val smapStr =
        s"""
           |:x@:S
         """.stripMargin
      val x: RDFNode = IRI(s"http://example.org/x")
      val s: ShapeMapLabel = IRILabel(IRI(s"http://example.org/S"))
      val eitherResult = for {
        rdf <- RDFAsJenaModel.fromChars(rdfStr,"TURTLE", None)
        shex <- Schema.fromString(shexStr,"SHEXC",None)
        shapeMap <- eitherStr2IO(ShapeMap.fromCompact(smapStr, None, rdf.getPrefixMap, shex.prefixMap))
        fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap, rdf, rdf.getPrefixMap, shex.prefixMap)
        resolved <- ResolvedSchema.resolve(shex,None)
        result <- Validator.validate(resolved, fixedShapeMap, rdf)
        resultShapeMap <- result.toResultShapeMap
      } yield resultShapeMap
      eitherResult.attempt.unsafeRunSync.fold(e =>
        fail(s"Error: $e"),
        r => {
          r.resultMap.get(x).getOrElse(Map()).get(s).fold{
            fail(s"$x has no value associated with $s in ${r.resultMap}")}(info =>
            println(s"${info.reason.getOrElse("")}")
          )
        }
      )
    }
  }
}
