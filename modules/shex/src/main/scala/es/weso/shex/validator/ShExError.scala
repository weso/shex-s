package es.weso.shex.validator
import cats._
import cats.implicits._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.shex._

sealed  abstract class ShExError extends Exception with Product with Serializable {
  def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String
}

object ShExError {

  def msgErr(msg: String): ShExError = StringError(msg)

  implicit def showViolationError: Show[ShExError] = new Show[ShExError] {
    override def show(e: ShExError): String = e match {
      case StringError(s) =>  s"Error: $s"
      case _ => e.toString
   }
  }

  case class StringError(msg: String) extends ShExError {
    override def toString: String =
      ShExError.showViolationError.show(this)

    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"Error: $msg"
    }
  }

  case class NotEnoughArcs(node: RDFNode,
                           values: Set[RDFNode],
                           path: Path,
                           min: Int
                          ) extends ShExError {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Not enough values for node: ${nodesPrefixMap.qualify(node)}
      Path: ${path.showQualified(shapesPrefixMap)}
      Values: ${values.map(nodesPrefixMap.qualify).mkString(",")}
      Min expected: $min"""
    }
  }

  case class LabelNotFound(label: ShapeLabel, availableLabels: List[ShapeLabel]) extends ShExError {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Label not found: ${shapesPrefixMap.qualify(label.toRDFNode)}
      Available labels: ${availableLabels.map(label => shapesPrefixMap.qualify(label.toRDFNode)).mkString(",")}"""
    }
  }

  case class NoStart(node: RDFNode) extends ShExError {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Checking node ${nodesPrefixMap.qualify(node)}@start but no start found"""
    }
  }

  case class ErrCardinality(attempt: Attempt, node: RDFNode, path: Path, values: Int, card: Cardinality) extends ShExError {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""${attempt.showQualified(nodesPrefixMap,shapesPrefixMap)}: # of values for ${path.showQualified(shapesPrefixMap)}=$values doesn't match ${card.show}"""
    }
  }

  case class ErrCardinalityWithExtra(attempt: Attempt, node: RDFNode, path: Path, values: Int, valuesFailed: Int, card: Cardinality) extends ShExError {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""${attempt.showQualified(nodesPrefixMap, shapesPrefixMap)}: # of values for ${path.showQualified(shapesPrefixMap)}=$values doesn't match ${card.show}
         | #of values that failed: $valuesFailed
         | """.stripMargin
    }
  }

  case class ValuesNotPassed(attempt: Attempt, node: RDFNode, path: Path, valuesPassed: Int, valuesFailed: Set[(RDFNode, String)]) extends ShExError {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""${attempt.showQualified(nodesPrefixMap, shapesPrefixMap)}: # of values for ${path.showQualified(shapesPrefixMap)} failed}
         | #values that failed: ${showValues(valuesFailed, nodesPrefixMap)}""".stripMargin
    }

    private def showValues(vs: Set[(RDFNode, String)], prefixMap: PrefixMap): String = {
      vs.map(pair => s"${prefixMap.qualify(pair._1)}: ${pair._2}").mkString("\n")
    }

  }

  case class ClosedButExtraPreds(preds: Set[IRI]) extends ShExError {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Closed shape but extra properties found: ${preds.map(shapesPrefixMap.qualifyIRI).mkString(",")}"""
    }
  }

  case class CheckDatatypeError(node: RDFNode, datatype: IRI) extends ShExError {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Node: ${nodesPrefixMap.qualify(node)} doesn't have datatype ${nodesPrefixMap.qualify(datatype)}"""
    }
  }

    // FractionDigits
    case class ErrorObtainingFractionDigits(node: RDFNode, e: Throwable) extends ShExError {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""FractionDigits(${nodesPrefixMap.qualify(node)}) Error: ${e.getMessage}"""
      }
    }

    case class FractionDigitsAppliedUnknownDatatype(node: RDFNode, d: IRI) extends ShExError {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""FractionDigits(${nodesPrefixMap.qualify(node)}) Error: Applied to wrong type: ${nodesPrefixMap.qualify(d)}"""
      }
    }
    case class FractionDigitsAppliedNonLiteral(node: RDFNode) extends ShExError {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""FractionDigits(${nodesPrefixMap.qualify(node)}) Error: applied to non literal"""
      }
    }

    // TotalDigits
    case class ErrorObtainingTotalDigits(node: RDFNode, e: Throwable) extends ShExError {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""TotalDigits(${nodesPrefixMap.qualify(node)}) Error: ${e.getMessage}"""
      }
    }
    case class TotalDigitsAppliedUnknownDatatype(node: RDFNode, d: IRI) extends ShExError {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""TotalDigits(${nodesPrefixMap.qualify(node)}) Error: Applied to wrong type: ${nodesPrefixMap.qualify(d)}"""
      }
    }

    case class TotalDigitsAppliedNonLiteral(node: RDFNode) extends ShExError {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""FractionDigits(${nodesPrefixMap.qualify(node)}) Error: applied to non literal"""
      }
    }

  case class ExtraPropertiesClosedShape(node: RDFNode, ps: List[IRI]) extends ShExError {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Closed shape with extra properties at node: ${nodesPrefixMap.qualify(node)}) Properties: ${showIris(nodesPrefixMap, ps)}"""
    }
  }

  case class FailSemanticAction(node: RDFNode, msg: String) extends ShExError {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Failed semantic action: ${nodesPrefixMap.qualify(node)}: $msg"""
    }
  }

  private def showIris(pm: PrefixMap, iris: List[IRI]): String =
    iris.map(pm.qualify(_)).mkString(",")

}

