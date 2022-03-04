package es.weso.shex.validator.validatorref

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
import es.weso.shex.validator.ExternalResolver
import es.weso.shex.validator.Validator
import es.weso.shex.validator.ShowValidator
import es.weso.shex.validator.Result


/**
  * ShEx validator with global state using ref
  */
case class ValidatorRef(
  schema: ResolvedSchema,
  externalResolver: ExternalResolver = ExternalResolver.NoAction,
  builder: RDFBuilder
 ) extends Validator with ShowValidator {

  
  

  // Public methods 

  /**
   * Validate a node against the START declaration
   **/
  def validateNodeStart(rdf: RDFReader, node: IRI, verbose: VerboseLevel): IO[Result] = 
    ???

  /**
   * Validate a node following target declarations.
   * This methods follows SHACL convention and could be deprecated in the future
   * 
   **/
  def validateNodeDecls(rdf: RDFReader, verbose: VerboseLevel): IO[Result] =
    ???
  
  /**
   * Validate a node against a shape
   **/
  def validateNodeShape(rdf: RDFReader, node: IRI, shape: String, verbose: VerboseLevel): IO[Result] = 
    ???

  /**
   * Validate a node against a shape map
   **/
  def validateShapeMap(rdf: RDFReader, 
                       shapeMap: FixedShapeMap, 
                       verbose: VerboseLevel): IO[Result] = 
    ???

}

