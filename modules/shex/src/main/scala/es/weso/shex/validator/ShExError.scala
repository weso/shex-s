package es.weso.shex.validator
import cats._
import cats.implicits._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.shex._
import io.circe.Encoder
import io.circe.Json
import es.weso.shex.validator.Table.CTable
import es.weso.rbe.{Shape => _, Attempt => _, _}
import es.weso.collection.Bag
import es.weso.rbe.BagChecker
import scala.util.control.NoStackTrace
import es.weso.rbe.ShowRbe._

sealed  abstract class ShExError protected (val msg: String) extends Exception(msg) with NoStackTrace with Product with Serializable {
  def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String
  def toJson: Json
}

object ShExError {

  def msgErr(msg: String): ShExError = StringError(msg)

  implicit def showViolationError: Show[ShExError] = new Show[ShExError] {
   override def show(e: ShExError): String = e match {
      case StringError(s) =>  s"Error: $s"
      case _ => e.showQualified(PrefixMap.empty,PrefixMap.empty)
   }
  }


  implicit def jsonEncoder: Encoder[ShExError] = new Encoder[ShExError] {
    override def apply(e: ShExError): Json = e match {
      case StringError(s) =>  Json.fromString(s"Error: $s")
      case _ => e.toJson
   }
  }

  case class StringError(override val msg: String) extends ShExError(msg) {
    override def toString: String =
      ShExError.showViolationError.show(this)

    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"Error: $msg"
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("StringError")),
       ("msg", Json.fromString(msg))
      ) 
  }

  case class NotEnoughArcs(node: RDFNode,
                           values: Set[RDFNode],
                           path: Path,
                           min: Int
                          ) extends ShExError(s"Not enough arcs for ${node}") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Not enough values for node: ${nodesPrefixMap.qualify(node)}
      Path: ${path.showQualified(shapesPrefixMap)}
      Values: ${values.map(nodesPrefixMap.qualify).mkString(",")}
      Min expected: $min"""
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("NotEnoughArcs")),
      ) 

  }

  case class LabelNotFound(label: ShapeLabel, availableLabels: List[ShapeLabel]) extends ShExError(s"Label not found: ${label}") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Label not found: ${shapesPrefixMap.qualify(label.toRDFNode)}
      Available labels: ${availableLabels.map(label => shapesPrefixMap.qualify(label.toRDFNode)).mkString(",")}"""
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("LabelNotFound")),
      ) 

  }

  case class NoStart(node: RDFNode) extends ShExError(s"No Start. Node $node") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Checking node ${nodesPrefixMap.qualify(node)}@start but no start found"""
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("NoStart")),
      ) 

  }

  case class ErrCardinality(attempt: Attempt, node: RDFNode, path: Path, values: Int, card: Cardinality) extends ShExError(s"Cardinality error. Node: $node. Cardinality: $card") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""${attempt.showQualified(nodesPrefixMap,shapesPrefixMap)}: # of values for ${path.showQualified(shapesPrefixMap)}=$values doesn't match ${card.show}"""
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("ErrCardinality")),
      ) 

  }

  case class ErrCardinalityWithExtra(attempt: Attempt, node: RDFNode, path: Path, values: Int, valuesFailed: Int, card: Cardinality) extends ShExError(s"Cardinality ${card} with extra. ${valuesFailed} failed. Values: ${values}") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""${attempt.showQualified(nodesPrefixMap, shapesPrefixMap)}: # of values for ${path.showQualified(shapesPrefixMap)}=$values doesn't match ${card.show}
         | #of values that failed: $valuesFailed
         | """.stripMargin
    }

   override def toJson: Json = Json.obj(
       ("type", Json.fromString("ErrCardinalityWithExtra")),
      ) 

  }

  case class ValuesNotPassed(attempt: Attempt, node: RDFNode, path: Path, valuesPassed: Int, valuesFailed: Set[(RDFNode, String)]
  ) extends ShExError(s"Error: ${valuesFailed} values failed. ${valuesPassed} values passed") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""${attempt.showQualified(nodesPrefixMap, shapesPrefixMap)}: # of values for ${path.showQualified(shapesPrefixMap)} failed}
         | #values that failed: ${showValues(valuesFailed, nodesPrefixMap)}""".stripMargin

    }

    private def showValues(vs: Set[(RDFNode, String)], prefixMap: PrefixMap): String = {
      vs.map(pair => s"${prefixMap.qualify(pair._1)}: ${pair._2}").mkString("\n")
    }


    override def toJson: Json = Json.obj(
       ("type", Json.fromString("ValuesNotPassed")),
      ) 

  }

  case class ClosedButExtraPreds(preds: Set[IRI]) extends ShExError(s"Closed but extra predicates: ${preds}") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Closed shape but extra properties found: ${preds.map(shapesPrefixMap.qualifyIRI).mkString(",")}"""
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("ClosedButExtraPreds")),
      ) 

  }

  case class CheckDatatypeError(node: RDFNode, datatype: IRI) extends ShExError(s"Check datatype error: ${node}. Datatype: ${datatype}") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Node: ${nodesPrefixMap.qualify(node)} doesn't have datatype ${nodesPrefixMap.qualify(datatype)}"""
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("CheckDatatypeError")),
      ) 

  }

    // FractionDigits
    case class ErrorObtainingFractionDigits(node: RDFNode, e: Throwable) extends ShExError(s"Error obtaining fraction digits: ${node}: ${e.getMessage()}") {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""FractionDigits(${nodesPrefixMap.qualify(node)}) Error: ${e.getMessage}"""
      }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("ErrorObtainingFractionDigits")),
      ) 

    }

    case class FractionDigitsAppliedUnknownDatatype(node: RDFNode, d: IRI) extends ShExError(s"Fraction digits applied to ${d} on node ${node}") {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""FractionDigits(${nodesPrefixMap.qualify(node)}) Error: Applied to wrong type: ${nodesPrefixMap.qualify(d)}"""
      }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("ErrorFractionDigitsAppliedUnknownDatatype")),
      ) 

    }
    case class FractionDigitsAppliedNonLiteral(node: RDFNode) extends ShExError(s"Fraction digits applied to non literal: $node") {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""FractionDigits(${nodesPrefixMap.qualify(node)}) Error: applied to non literal"""
      }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("FractionDigitsAppliedNonLiteral")),
      ) 

    }

    // TotalDigits
    case class ErrorObtainingTotalDigits(node: RDFNode, e: Throwable) extends ShExError(s"Error obtaining total digits: ${node}: ${e.getMessage()}") {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""TotalDigits(${nodesPrefixMap.qualify(node)}) Error: ${e.getMessage}"""
      }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("ErrorObtainingTotalDigits")),
      ) 
    }
    case class TotalDigitsAppliedUnknownDatatype(node: RDFNode, d: IRI) extends ShExError(s"Total digits applied to unknown datatye: ${d}") {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""TotalDigits(${nodesPrefixMap.qualify(node)}) Error: Applied to wrong type: ${nodesPrefixMap.qualify(d)}"""
      }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("TotalDigitsAppliedUnknownDatatype")),
      ) 

    }

    case class TotalDigitsAppliedNonLiteral(node: RDFNode) extends ShExError(s"Total digits applied to non literal: ${node}") {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""FractionDigits(${nodesPrefixMap.qualify(node)}) Error: applied to non literal"""
      }

      override def toJson: Json = Json.obj(
       ("type", Json.fromString("TotalDigitsAppliedNonLiteral")),
      ) 

    }

  case class ExtraPropertiesClosedShape(node: RDFNode, ps: List[IRI]) extends ShExError(s"EXTRA properties on closed shape: ${ps}") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Closed shape with extra properties at node: ${nodesPrefixMap.qualify(node)}) Properties: ${showIris(nodesPrefixMap, ps)}"""
    }
    override def toJson: Json = Json.obj(
       ("type", Json.fromString("ExtraPropertiesClosedShape")),
      ) 

  }

  case class FailSemanticAction(node: RDFNode, override val msg: String) extends ShExError(s"Failed semantic action on node: $node: $msg") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Failed semantic action: ${nodesPrefixMap.qualify(node)}: $msg"""
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("FailSemanticAction")),
      ) 
  }

  case class ErrRBEMatch(attempt: Attempt, 
    cl: CandidateLine, 
    table: CTable, 
    bag: Bag[ConstraintRef], 
    rbe: Rbe[ConstraintRef],
    err: RbeError
    ) extends ShExError(s"Error matching RBE: ${err.msg}") {

    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""|Error matching expression.
          | Error: ${err}
          | Attempt: ${attempt.show} 
          | Candidate line:
          | ${showCandidateLine(cl,table)} 
          |  which corresponds to bag:
          |  ${bag} 
          | does not match expression: 
          |  ${rbe.show}
          | Table:${table.show} """.stripMargin
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("ErrorMatchingRegularExpression")),
       ("node", Json.fromString(attempt.nodeShape.node.getLexicalForm)),
       ("error", err.toJson),
       ("shape", Json.fromString(attempt.nodeShape.shape.label.map(_.toRDFNode.getLexicalForm).getOrElse("?"))),
       ("bag", Json.fromString(bag.toString)),
       ("regularExpression",Json.fromString(Rbe.show(rbe))),
       ("candidateLine",cl.toJson),
       ("table", table.toJson)
      ) 
  }

  case class NoCandidate(attempt: Attempt, 
       bagChecker: BagChecker[ConstraintRef], 
       as: List[CandidateLine], 
       ctable: CTable
      ) extends ShExError(s"No candidate matches") {
    
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""|None of the candidates matched.
          | Attempt: ${attempt.show}
          | Candidate lines:\n${showCandidateLines(as,ctable)}
          |""".stripMargin
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("NoCandidate")),
      ) 
  }

  case class NoPartition(
    attempt: Attempt, 
    node: RDFNode, 
    shape: Shape, 
    label: ShapeLabel,
    neighs: List[Arc]
    ) extends ShExError(s"No partition matches") {

    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""No partition found for node ${nodesPrefixMap.qualify(node)}
          Attempt: ${attempt.show} 
        """"
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("NoPartition")),
       ("node", Json.fromString(node.getLexicalForm))
      ) 
  }
  
  private def showCandidateLines(cs: List[CandidateLine], table: CTable): String = {
    cs.length match {
      case 0 => "No candidate lines"
      case 1 => s"One candidate line\n${showCandidateLine(cs.head, table)}"
      case _ => cs.map(showCandidateLine(_,table)).mkString("\n")
    }
  }


  private def showCandidateLine(c: CandidateLine, table: CTable): String = {
      def compare(pair1:(Arc,ConstraintRef), pair2:(Arc,ConstraintRef)): Boolean =
        Ordering[ConstraintRef].compare(pair1._2, pair2._2) <= 0

      s"Candidate line:\n${c.values.sortWith(compare).map{ 
        case (arc,cref) => 
        s"${arc.show} as ${cref.show}/${table.constraints.get(cref).map(_.show).getOrElse("?")}"
      }.mkString("\n")}"
  }

  private def showIris(pm: PrefixMap, iris: List[IRI]): String =
    iris.map(pm.qualify(_)).mkString(",")

}

  case class SemanticActionException(attempt: Attempt, node: RDFNode, action: SemAct, exc: Throwable) extends ShExError(s"Semantic Action exception: ${exc.getMessage()}") {
    
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""|Semantic action exception: ${exc.getMessage()}
          |Action IRI: ${action.name}
          |Action code: ${action.code}
          |Node: ${nodesPrefixMap.qualify(node)}
          |Attempt: ${attempt} 
          |""".stripMargin
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("SemanticActionError")),
       ("message", Json.fromString(exc.getMessage())),
       ("action", Json.obj(
         ("iri", Json.fromString(action.name.toString())),
         ("code", Json.fromString(action.code.getOrElse("")))
       )),
       ("node", Json.fromString(node.getLexicalForm))
      )
  }
