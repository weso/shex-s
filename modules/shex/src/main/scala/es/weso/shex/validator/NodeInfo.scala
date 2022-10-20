package es.weso.shex.validator

import cats.implicits._
import cats.effect.IO
import es.weso.rdf.nodes.{Literal, RDFNode}
import es.weso.rdf.PREFIXES._
import es.weso.rdf.RDFReader
import es.weso.rdf.operations.Comparisons._
import es.weso.shex.validator.ShExError._
import scala.util._

object NodeInfo {

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
              td <- IO.fromEither(numericValue(node).map(_.totalDigits()))
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
              n <- IO.fromEither(numericValue(node).map(_.fractionDigits()))
            } yield n
          case d => IO.raiseError(FractionDigitsAppliedUnknownDatatype(node, d))
        }
      case _ => IO.raiseError(FractionDigitsAppliedNonLiteral(node))
    }

  def length(node: RDFNode): Int = node.getLexicalForm.length

}
