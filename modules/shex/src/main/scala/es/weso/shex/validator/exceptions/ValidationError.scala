package es.weso.shex.validator.exceptions

import es.weso.rdf.nodes._

sealed abstract class ValidationError extends Exception with Product with Serializable

object ValidationError {

  // FractionDigits
  case class ErrorObtainingFractionDigits(node: RDFNode, e: Throwable) extends ValidationError
  case class FractionDigitsAppliedUnknownDatatype(node: RDFNode, d: IRI) extends ValidationError
  case class FractionDigitsAppliedNonLiteral(node: RDFNode) extends ValidationError

  // TotalDigits
  case class ErrorObtainingTotalDigits(node: RDFNode, e: Throwable) extends ValidationError
  case class TotalDigitsAppliedUnknownDatatype(node: RDFNode, d: IRI) extends ValidationError
  case class TotalDigitsAppliedNonLiteral(node: RDFNode) extends ValidationError

  // Datatype
  case class CheckDatatypeError(node: RDFNode,datatype: IRI) extends ValidationError

}
