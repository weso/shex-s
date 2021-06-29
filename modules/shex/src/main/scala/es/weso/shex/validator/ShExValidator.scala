package es.weso.shex.validator

import es.weso.rdf._ 
import es.weso.rdf.nodes._ 
import es.weso.shapemaps._
import cats.effect._
import es.weso.shex._

case class ValidationOptions(
    verbose: Boolean,
    builder: Option[RDFBuilder]
)

abstract class ShExValidator {

  /**
    * Get ShEx schema used to validate
    */  
  def schema: Schema  
  
  /**
    * Validate rdf agains a shape map
    *
    * @param rdf input rdf data
    * @param shapeMap input shape map
    * @param options validation options
    * @return resulting shape map
    */  
  def validateShapeMap(rdf: RDFReader, shapeMap: ShapeMap, options: ValidationOptions): IO[ResultShapeMap]

  /**
    * Validate a node against a specific shape
    *
    * @param rdf
    * @param node
    * @param shape
    * @return resulting shape map
    */
  def validateNodeShape(rdf: RDFReader, node: RDFNode, shape: ShapeLabel, options: ValidationOptions): IO[ResultShapeMap]

  /**
    * Validate node against Start shape in a Schema
    *
    * @param rdf rdf data
    * @param node node to validate
    * @return resulting shape map
    */
  def validateNodeStart(rdf: RDFReader, node: RDFNode, options: ValidationOptions): IO[ResultShapeMap]

}