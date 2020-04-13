package es.weso.shex.validator

import es.weso.rdf.jena._
import es.weso.shapeMaps.ShapeMap
import es.weso.shex._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import es.weso.utils.eitherios.EitherIOUtils._

class ExprTest extends AnyFunSpec with Matchers with EitherValues {

  describe(s"Parsing exprs") {
    ignore("should parse a simple as") {
      val strSchema =
        """
          |prefix : <http://example.org/>
          |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
          |
          |:R {
          | :a xsd:integer as ?a
          |}
        """.stripMargin

      val eitherResult = for {
        schema <- Schema.fromString(strSchema, "ShExC", None)
      } yield schema

      eitherResult.attempt.unsafeRunSync.fold(
        e => fail(s"Error: $e"), 
        r => info(s"Parsed as $r"))
    }
  }

  ignore("Expr test") {
    it(s"Should validate triple with expr") {
      val strSchema =
        """prefix : <http://example.org/>
          |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
          |
          |:R {
          | :b  xsd:integer as $b;
          | :b1 xsd:integer as $b1 ;
          | $b1 = $b + 1
          |}
        """.stripMargin

      val strRdf =
        """
          |prefix : <http://example.org/>
          |
          |:good :b 1 ; :b1 2 .
          |:bad  :b 1 ; :b1 3 .
        """.stripMargin

      val strShapeMap =
        """
          |:good@:R,:bad@:R
        """.stripMargin

      // TODO: Change value of bad to !:R
      val strExpectedShapeMap =
        """
          |:good@:R,:bad@:R
        """.stripMargin

      val eitherResult = for {
        rdf <- RDFAsJenaModel.fromChars(strRdf,"TURTLE",None)
        schema <- Schema.fromString(strSchema,"ShExC",None)
        shapeMap <- eitherStr2IO(ShapeMap.fromString(strShapeMap,"Compact",None,rdf.getPrefixMap,schema.prefixMap))
        fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap,rdf,rdf.getPrefixMap,schema.prefixMap)
        expectedShapeMap <- eitherStr2IO(ShapeMap.fromString(strExpectedShapeMap,"Compact", None, rdf.getPrefixMap,schema.prefixMap))
        resolvedSchema <- ResolvedSchema.resolve(schema,None)
        result <- Validator.validate(resolvedSchema,fixedShapeMap,rdf)
        expectedShapeMap <- ShapeMap.parseResultMap(strExpectedShapeMap, None, rdf, schema.prefixMap)
        resultShapeMap <- result.toResultShapeMap
        compare <- eitherStr2IO(expectedShapeMap.compareWith(resultShapeMap))
      } yield result

      eitherResult.attempt.unsafeRunSync.fold(
        e => fail(s"Error: $e"),
        r => info(s"Result: $r")
      )
    }
  }


}
