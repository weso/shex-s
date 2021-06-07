package es.weso.shexs

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex._
import es.weso.shapemaps._
import es.weso.shex.validator._
import cats.effect._
import cats.implicits._
import es.weso.utils.IOUtils._
// import java.io.Reader
import java.io.InputStream
import java.io.InputStreamReader
import cats.effect.unsafe.implicits.global

object ShExS {


    def validate(
        data: InputStream, 
        schema: InputStream, 
        shapeMap: InputStream, 
        options: ShExsOptions
    ): ResultShapeMap = {
      val dataReader = new InputStreamReader(data)

      val cmp: IO[ResultShapeMap] = for {
          res1 <- RDFAsJenaModel.fromReader(dataReader,options.dataFormat,options.base)
          res2 <- RDFAsJenaModel.empty
          v <- (res1,res2).tupled.use {
              case (rdf,builder) => for {
                prefixMap <- rdf.getPrefixMap
                schema <- Schema.fromInputStream(schema, options.schemaFormat, options.base,None)
                resolved <- ResolvedSchema.resolve(schema, options.base)
                shapeMap <- fromES(
                  ShapeMap.fromInputStream(shapeMap, options.shapemapFormat,options.base, prefixMap,resolved.prefixMap
                  ).leftMap(es => es.toList.mkString("\n")))
                fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap,rdf,prefixMap,schema.prefixMap)
                result <- Validator.validate(resolved,fixedShapeMap,rdf,builder,options.verbose)
                resultShapeMap <- result.toResultShapeMap
              } yield resultShapeMap
          }
      } yield v 
      cmp.unsafeRunSync()
    }



    /**
      * Validate RDF data using ShEx
      *
      * @param dataStr string containing RDF data
      * @param schemaStr string containing ShEx schema
      * @param shapeMapStr string containing shape map
      * @param options object representing validation options
      * @return resultShapeMap
      */
    def validate(
        dataStr: String, 
        schemaStr: String, 
        shapeMapStr: String, 
        options: ShExsOptions
    ): ResultShapeMap = {

      val cmp: IO[ResultShapeMap] = for {
          res1 <- RDFAsJenaModel.fromString(dataStr,options.dataFormat,options.base)
          res2 <- RDFAsJenaModel.empty
          v <- (res1,res2).tupled.use {
              case (rdf,builder) => for {
                prefixMap <- rdf.getPrefixMap      
                schema <- Schema.fromString(schemaStr, options.schemaFormat, options.base,None)
                resolved <- ResolvedSchema.resolve(schema, options.base)
                shapeMap <- fromES(
                  ShapeMap.fromString(shapeMapStr, options.shapemapFormat,options.base, prefixMap,resolved.prefixMap).leftMap(_.toString.mkString("\n"))
                )
                fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap,rdf,prefixMap,schema.prefixMap)
                result <- Validator.validate(resolved,fixedShapeMap,rdf,builder,options.verbose)
                resultShapeMap <- result.toResultShapeMap
              } yield resultShapeMap
          }
      } yield v 
      cmp.unsafeRunSync()
    }

    /**
      * Validate RDF data using ShEx
      *
      * @param dataStr string containing RDF data
      * @param schemaStr string containing ShEx schema
      * @param node IRI of RDF node to check conformance
      * @param shape IRI of shape label target shape label
      * @param options object representing validation options
      * @return resultShapeMap
      */
    def validateNodeShape(
        dataStr: String, 
        schemaStr: String, 
        node: String, 
        shape: String,
        options: ShExsOptions
    ): ResultShapeMap = {
      validate(dataStr,schemaStr,s"<$node>@<$shape>",options)
    }

}