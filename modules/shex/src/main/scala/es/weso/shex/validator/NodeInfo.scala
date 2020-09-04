package es.weso.shex.validator

import cats.data._
import cats.syntax.all._
import cats.effect.IO
import es.weso.rdf.nodes.{Literal, RDFNode}
import es.weso.rdf.PREFIXES._
import es.weso.rdf.RDFReader
import org.apache.xerces.impl.dv.{SchemaDVFactory, ValidatedInfo, XSSimpleType}
import org.apache.xerces.impl.dv.xs.DecimalDV
import org.apache.xerces.impl.validation.ValidationState
import es.weso.utils.IOUtils._

import scala.util._

object NodeInfo {

  /* This implementation leverages Xerces internal implementation of XML Schema datatypes */
  /* This is probably going too far and could be simplified */
  def totalDigits(node: RDFNode, rdf: RDFReader): EitherT[IO,String,Int] = {
    node match {
      case l: Literal => l.dataType match {
        case `xsd:decimal` |
             // Here we include also the derived types from xsd:decimal according to https://www.w3.org/TR/xmlschema-2/#decimal
             `xsd:integer` |
             `xsd:nonPositiveInteger` |
             `xsd:negativeInteger` |
             `xsd:nonNegativeInteger` |
             `xsd:long` |
             `xsd:unsignedLong` |
             `xsd:positiveInteger` |
             `xsd:unsignedInt` |
             `xsd:unsignedShort` |
             `xsd:unsignedByte` |
             `xsd:int` |
             `xsd:short` |
             `xsd:byte` => for {
          b <- fromIOEither(rdf.checkDatatype(node,l.dataType).attempt)
          td <- {
            val t = Try {
              val context                       = new ValidationState
              val decimalDV                     = new DecimalDV()
              val typeDeclaration: XSSimpleType = SchemaDVFactory.getInstance.getBuiltInType("decimal")
              val resultInfo                    = new ValidatedInfo
              typeDeclaration.validate(node.getLexicalForm, context, resultInfo)
              decimalDV.getTotalDigits(resultInfo.actualValue)
            }
            t match {
              case Failure(e) => fail_es(s"Error calculating totalDigits of $node: ${e.getMessage}")
              case Success(n) => ok_es(n)
            }
          }
        } yield td
        case d => fail_es(s"TotalDigits can only be applied to xsd:decimal or derived datatypes, not to: $d")
      }
      case _ => fail_es(s"TotalDigits facet can not be applied to non literal node: $node")
    }
  }

  def fromIOEither[A](e:IO[Either[Throwable,A]]): EitherT[IO,String,A] = {
   EitherT(e.map(_.leftMap(_.getMessage)))
  }

  /* This implementation leverages Xerces internal implementation of XML Schema datatypes */
  /* This is probably going too far and could be simplified */
  def fractionDigits(node: RDFNode, rdf: RDFReader): EitherT[IO,String,Int] = {
    node match {
      case l: Literal =>
        l.dataType match {
          case `xsd:decimal` | `xsd:integer` => for {
            b <- fromIOEither(rdf.checkDatatype(node,l.dataType).attempt)
            td <- {
             val t = Try {
               val context                       = new ValidationState
               val decimalDV                     = new DecimalDV()
               val typeDeclaration: XSSimpleType = SchemaDVFactory.getInstance.getBuiltInType("decimal")
               val resultInfo                    = new ValidatedInfo
               typeDeclaration.validate(node.getLexicalForm, context, resultInfo)
               decimalDV.getFractionDigits(resultInfo.actualValue)
             }
             t match {
               case Failure(e) => fail_es(s"Error calculating fractionDigits of $node: ${e.getMessage}")
               case Success(n) => ok_es(n)
             }
            }
           } yield td
          case d => fail_es(s"FractionDigits can only be applied to xsd:decimal or derived datatypes, not to: $d")
        }
      case _ => fail_es(s"FractionDigits facet can not be applied to non literal node: $node")
    }
  }

  def length(node: RDFNode): Int = node.getLexicalForm.length

}