package es.weso.shex.validator

import cats.implicits._
import cats.effect.IO
import es.weso.rdf.nodes.{Literal, RDFNode}
import es.weso.rdf.PREFIXES._
import es.weso.rdf.RDFReader
import es.weso.shex.validator.ShExError._
import org.apache.xerces.impl.dv.{SchemaDVFactory, ValidatedInfo, XSSimpleType}
import org.apache.xerces.impl.dv.xs.DecimalDV
import org.apache.xerces.impl.validation.ValidationState
import scala.util._

object NodeInfo {

  /* This implementation leverages Xerces internal implementation of XML Schema datatypes */
  /* This is probably going too far and could be simplified */
  def totalDigits(node: RDFNode, rdf: RDFReader): IO[Int] =
    node match {
      case l: Literal =>
        l.dataType match {
          case `xsd:decimal` |
              // Here we include also the derived types from xsd:decimal according to https://www.w3.org/TR/xmlschema-2/#decimal
              `xsd:integer` | `xsd:nonPositiveInteger` | `xsd:negativeInteger` |
              `xsd:nonNegativeInteger` | `xsd:long` | `xsd:unsignedLong` | `xsd:positiveInteger` |
              `xsd:unsignedInt` | `xsd:unsignedShort` | `xsd:unsignedByte` | `xsd:int` |
              `xsd:short` | `xsd:byte` =>
            for {
              eitherB <- rdf.checkDatatype(node, l.dataType).attempt
              _ <- eitherB.fold(
                e => IO.raiseError(CheckDatatypeError(node, l.dataType, rdf)),
                b =>
                  if (b) IO(())
                  else IO.raiseError(CheckDatatypeError(node, l.dataType, rdf))
              )
              td <- getTotalDigits(node.getLexicalForm)
            } yield td
          case d => IO.raiseError(TotalDigitsAppliedUnknownDatatype(node, d))
        }
      case _ => IO.raiseError(TotalDigitsAppliedNonLiteral(node))
    }

  def fractionDigits(node: RDFNode, rdf: RDFReader): IO[Int] =
    node match {
      case l: Literal =>
        l.dataType match {
          case `xsd:decimal` | `xsd:integer` =>
            for {
              eitherB <- rdf.checkDatatype(node, l.dataType).attempt
              _ <- eitherB.fold(
                e => IO.raiseError(CheckDatatypeError(node, l.dataType, rdf)),
                b =>
                  if (b) IO(())
                  else IO.raiseError(CheckDatatypeError(node, l.dataType, rdf))
              )
              // _ <- { pprint.log(node.getLexicalForm); IO(()) }
              n <- getFractionDigits(node.getLexicalForm)
            } yield n
          case d => IO.raiseError(FractionDigitsAppliedUnknownDatatype(node, d))
        }
      case _ => IO.raiseError(FractionDigitsAppliedNonLiteral(node))
    }

  // TODO Remove dependency on Xerces
  def getTotalDigits(value: String): IO[Int] = Try {
    val context = new ValidationState
    val decimalDV = new DecimalDV()
    val typeDeclaration: XSSimpleType = SchemaDVFactory.getInstance.getBuiltInType("decimal")
    val resultInfo = new ValidatedInfo
    typeDeclaration.validate(value, context, resultInfo)
    decimalDV.getTotalDigits(resultInfo.actualValue)
  }.fold(
    e => IO.raiseError(ErrorObtainingTotalDigits(value, e)),
    n => n.pure[IO]
  )

  // TODO replace this by a builtin implementation
  /* This implementation leverages Xerces internal implementation of XML Schema datatypes */
  /* This is probably going too far and could be simplified */
  def getFractionDigits(value: String): IO[Int] =
    Try {
      val context = new ValidationState
      val decimalDV = new DecimalDV()
      val typeDeclaration: XSSimpleType = SchemaDVFactory.getInstance.getBuiltInType("decimal")
      val resultInfo = new ValidatedInfo
      typeDeclaration.validate(value, context, resultInfo)
      decimalDV.getFractionDigits(resultInfo.actualValue)
    }.fold(
      e => IO.raiseError(ErrorObtainingFractionDigits(value, e)),
      n => n.pure[IO]
    )

  def length(node: RDFNode): Int = node.getLexicalForm.length

}
