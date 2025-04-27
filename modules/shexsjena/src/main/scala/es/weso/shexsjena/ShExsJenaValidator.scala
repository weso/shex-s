package es.weso.shexsjena

import es.weso.shex.Schema
import es.weso.rdf.nodes.RDFNode
import es.weso.shex.ShapeLabel
import es.weso.shapemaps.ResultShapeMap
import es.weso.shex.ResolvedSchema
import es.weso.shex.validator._
import cats.effect.IO
import es.weso.rdf.jena._
import es.weso.shex.validator.ExternalResolver._
import org.apache.jena.rdf.model.Model
import org.apache.jena.iri.{IRI => JenaIRI}
import es.weso.rdf.nodes.IRI
import cats._
import cats.data._
import cats.implicits._
import cats.implicits._
import cats.effect.unsafe.implicits.global
import es.weso.utils.IOUtils._
import es.weso.shapemaps._
import cats.data.NonEmptyList
import es.weso.utils.VerboseLevel
import java.io.InputStream

case class ShExsJenaValidator(schema: Schema) {

  /** Validates a node with a shape label in the schema
    */
  def validateNodeShapeSync(
      model: Model,
      node: String,
      shape: String
  ): ResultShapeMap = {
    val verbose = VerboseLevel.Nothing
    val cmp: IO[ResultShapeMap] = RDFAsJenaModel.empty.flatMap(
      _.use(builder =>
        for {
          nodeIri <- fromES(IRI.fromString(node))
          rdf <- RDFAsJenaModel.fromModel(model, None, None, Map())
          resolvedSchema <- ResolvedSchema.resolve(schema, None, verbose)
          validator = Validator(resolvedSchema, NoAction, builder)
          result <- validator.validateNodeShape(rdf, nodeIri, shape, verbose)
          resultShapeMap <- result.toResultShapeMap
        } yield resultShapeMap
      )
    )
    cmp.unsafeRunSync()
  }

  def validateShapeMapSync(
      model: Model,
      shapeMap: String
  ): ResultShapeMap = {
    val verbose = VerboseLevel.Nothing
    val cmp: IO[ResultShapeMap] = RDFAsJenaModel.empty.flatMap(
      _.use(builder =>
        for {
          rdf <- RDFAsJenaModel.fromModel(model, None, None, Map())
          resolvedSchema <- ResolvedSchema.resolve(schema, None, verbose)
          validator = Validator(resolvedSchema, NoAction, builder)
          rdfPrefixMap <- rdf.getPrefixMap
          shapeMap <- fromESNel(
            ShapeMap.fromString(shapeMap, "COMPACT", None, rdfPrefixMap, schema.prefixMap)
          )
          fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap, rdf, rdfPrefixMap, schema.prefixMap)
          result <- validator.validateShapeMap(rdf, fixedShapeMap, verbose)
          resultShapeMap <- result.toResultShapeMap
        } yield resultShapeMap
      )
    )
    cmp.unsafeRunSync()
  }

  def fromESNel[A](e: Either[NonEmptyList[String], A]): IO[A] =
    fromES(e.leftMap(es => es.toList.mkString(",")))
}
