package es.weso.shex.validator

import cats._
import implicits._
import cats.effect.IO
import es.weso.shex._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rbe.interval.IntervalChecker
import es.weso.rbe.Empty
import es.weso.utils.SetUtils
import es.weso.shex.implicits.showShEx._
import es.weso.shapemaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMapLabel, _}
import es.weso.shex.actions.TestSemanticAction
import Function.tupled
import es.weso.shex.validator.ShExError._
import es.weso.shex.validator.ConstraintRef.{showConstraintRef => _}
import es.weso.utils.internal.CollectionCompat._
import es.weso.utils.VerboseLevel
import PartitionUtils._


/**
  * ShEx validator
  */
abstract class Validator {

  /**
   * Validate a node against the START declaration
   **/
  def validateNodeStart(rdf: RDFReader, node: IRI, verbose: VerboseLevel): IO[Result] 

  /**
   * Validate a node following target declarations.
   * This methods follows SHACL convention and could be deprecated in the future
   * 
   **/
  def validateNodeDecls(rdf: RDFReader, verbose: VerboseLevel): IO[Result] 
  
  /**
   * Validate a node against a shape
   **/
  def validateNodeShape(rdf: RDFReader, node: IRI, shape: String, verbose: VerboseLevel): IO[Result] 

  /**
   * Validate a node against a shape map
   **/
  def validateShapeMap(rdf: RDFReader, 
                       shapeMap: FixedShapeMap, 
                       verbose: VerboseLevel): IO[Result] 

}

object Validator {

  def empty(builder: RDFBuilder): IO[Validator] = for {
    schema <- ResolvedSchema.empty
  } yield ValidatorEitherT(schema = schema, builder = builder)

  /**
    * Validate RDF according to a Shapes Schema
    *
    * @param schema: ShEx schema
    * @param fixedShapeMap: Shape map
    * @param rdf: RDF to validate
    * @param builder: RDF builder to return subgraph validated
    * @return Result of validation
    */
  def validate(schema: ResolvedSchema, 
               fixedShapeMap: FixedShapeMap, 
               rdf: RDFReader, 
               builder: RDFBuilder,
               verbose: VerboseLevel
              ): IO[Result] = {
    val validator = ValidatorEitherT(schema, ExternalResolver.NoAction, builder)
    validator.validateShapeMap(rdf, fixedShapeMap, verbose)
  } 

  /**
   * Build a validator
   * @param schema: ResolvedSchema
   * @param externalResolver: External resolver
   * @param build RDF builder that returns the subgraph validated
   **/
  def apply(schema: ResolvedSchema, 
            externalResolver: ExternalResolver = ExternalResolver.NoAction,
            builder: RDFBuilder): Validator = 
    ValidatorEitherT(schema,externalResolver,builder)

}



