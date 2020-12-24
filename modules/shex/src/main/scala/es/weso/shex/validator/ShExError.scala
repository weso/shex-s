package es.weso.shex.validator
import cats._
import cats.implicits._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.shex._
import io.circe._
import io.circe.syntax._
import es.weso.rbe.{Shape => _, Attempt => _, _}
import es.weso.collection.Bag
import es.weso.rbe.BagChecker
import scala.util.control.NoStackTrace
import es.weso.rbe.ShowRbe._
import es.weso.shex.implicits.encoderShEx._
import Attempt._
import es.weso.rdf.RDFReader
import es.weso.rdf.locations.Location
import es.weso.rdf.nodes.Literal
import es.weso.rdf.nodes.DatatypeLiteral

sealed  abstract class ShExError protected (val msg: String) extends Exception(msg) with NoStackTrace with Product with Serializable {
  def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String
  def toJson: Json

  override def toString: String = s"err: $msg"
}

object ShExError {

  def node2Json(node: RDFNode, rdf: RDFReader): Json = {
    val node2search = node match {
      case l: Literal => DatatypeLiteral(l.getLexicalForm,l.dataType)
      case _ => node
    }
    val locations = rdf.nodeLocations.get(node2search) 
    Json.fromFields(
     List(("lexicalForm", node.getLexicalForm.asJson)) 
       ++ (if (locations.isEmpty) List()
              else List(("location", locations.toList.asJson))))
  }

  implicit val locationEncoder: Encoder[Location] = new Encoder[Location] {
    
    final def apply(loc: Location): Json = {
      Json.fromFields(
        List(
         ("line", loc.line.asJson),
         ("col", loc.col.asJson),
         ("type",loc.tokenType.asJson),
        ) ++ (loc.source match {
         case None => List()
         case Some(iri) => List(("source", iri.str.asJson))
        }))
    }
  }


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
                           min: Int,
                           rdf: RDFReader
                          ) extends ShExError(s"Not enough arcs for ${node}") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Not enough values for node: ${nodesPrefixMap.qualify(node)}
      Path: ${path.showQualified(shapesPrefixMap)}
      Values: ${values.map(nodesPrefixMap.qualify).mkString(",")}
      Min expected: $min"""
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("NotEnoughArcs")) ,
       ("node", node2Json(node,rdf)),
       ("path", Json.fromString(path.pred.getLexicalForm))
      ) 

  }

  case class LabelNotFound(
    label: ShapeLabel, 
    availableLabels: List[ShapeLabel]
    ) extends ShExError(s"Label not found: ${label}") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Label not found: ${shapesPrefixMap.qualify(label.toRDFNode)}
      Available labels: ${availableLabels.map(label => shapesPrefixMap.qualify(label.toRDFNode)).mkString(",")}"""
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("LabelNotFound")),
      ) 

  }

  case class NoStart(node: RDFNode, rdf: RDFReader) extends ShExError(s"No Start. Node $node") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Checking node ${nodesPrefixMap.qualify(node)}@start but no start found"""
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("NoStart")),
       ("node", node2Json(node,rdf))
      ) 

  }

  case class ErrCardinality(
     attempt: Attempt, node: RDFNode, path: Path, values: Int, card: Cardinality, rdf: RDFReader) extends ShExError(s"Cardinality error. Node: $node. Cardinality: $card") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""${attempt.showQualified(nodesPrefixMap,shapesPrefixMap)}: # of values for ${path.showQualified(shapesPrefixMap)}=$values doesn't match ${card.show}"""
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("ErrCardinality")),
       ("node", node2Json(node,rdf)),
       ("attempt", attempt.asJson)
      ) 

  }

  case class ErrCardinalityWithExtra(attempt: Attempt, node: RDFNode, path: Path, values: Int, valuesFailed: Int, card: Cardinality, rdf: RDFReader) 
    extends ShExError(s"Cardinality ${card} with extra. ${valuesFailed} failed. Values: ${values}") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""${attempt.showQualified(nodesPrefixMap, shapesPrefixMap)}: # of values for ${path.showQualified(shapesPrefixMap)}=$values doesn't match ${card.show}
         | #of values that failed: $valuesFailed
         | """.stripMargin
    }

   override def toJson: Json = Json.obj(
       ("type", Json.fromString("ErrCardinalityWithExtra")),
       ("node", node2Json(node,rdf)),
       ("attempt", attempt.asJson)
      ) 

  }

  case class ValuesNotPassed(
    attempt: Attempt, 
    node: RDFNode, 
    path: Path, 
    valuesPassed: Int, 
    valuesFailed: Set[(RDFNode, String)],
    rdf: RDFReader
  ) extends ShExError(s"""|Error for node ${node.getLexicalForm}: 
                          |${valuesFailed} values failed. 
                          |${valuesPassed} values passed
                          |""".stripMargin) {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""|${attempt.showQualified(nodesPrefixMap, shapesPrefixMap)}: # of values for ${path.showQualified(shapesPrefixMap)} failed}
          | #values that failed: ${showValues(valuesFailed, nodesPrefixMap)}""".stripMargin

    }

    private def showValues(vs: Set[(RDFNode, String)], prefixMap: PrefixMap): String = {
      vs.map(pair => s"${prefixMap.qualify(pair._1)}: ${pair._2}").mkString("\n")
    }


    override def toJson: Json = Json.obj(
       ("type", Json.fromString("ValuesNotPassed")),
       ("node", node2Json(node,rdf)),
       ("attempt", attempt.asJson)
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

  case class CheckDatatypeError(node: RDFNode, datatype: IRI, rdf: RDFReader) 
   extends ShExError(s"Check datatype error: ${node}. Datatype: ${datatype}") {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""Node: ${nodesPrefixMap.qualify(node)} doesn't have datatype ${nodesPrefixMap.qualify(datatype)}"""
    }

    override def toJson: Json = { 
     println(s"NodeLocations:${rdf.nodeLocations}") 
     println(s"Node:${node} ${node.getClass().getName()}") 
     Json.fromFields(
      List(("type", Json.fromString("CheckDatatypeError")),
           ("node", node2Json(node,rdf))
      ))
    }
  }

    // FractionDigits
  case class ErrorObtainingFractionDigits(value: String, e: Throwable) extends ShExError(s"Error obtaining fraction digits: ${value}: ${e.getMessage()}") {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""FractionDigits(${value}) Error: ${e.getMessage}"""
      }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("ErrorObtainingFractionDigits")),
       ("error", Json.fromString(e.getMessage)),
       ("value", Json.fromString(value)),
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
    case class ErrorObtainingTotalDigits(value: String, e: Throwable) extends ShExError(s"Error obtaining total digits: ${value}: ${e.getMessage()}") {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""TotalDigits(${value}) Error: ${e.getMessage}"""
      }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("ErrorObtainingTotalDigits")),
       ("value", Json.fromString(value)),
       ("error", Json.fromString(e.getMessage()))
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
       ("table", table.toJson),
       ("attempt", attempt.asJson)
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

  case class NoCandidateLine(
    attempt: Attempt,
    table: CTable) extends ShExError(s"No candidate line found: ${attempt.show}") {
    
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""|No candidates found to match
          |Atempt: ${attempt.show}
          |Table: ${table.show}
          |""".stripMargin
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("NoCandidateLine")),
       ("attempt", attempt.asJson),
       ("table", table.asJson)
      )
  }


  case class AbstractShapeErr(node: RDFNode, shape: ShapeExpr) extends ShExError(s"Node ${node.show} cannot conform to abstract shape ${shape}") {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""AbstractShapeError ${nodesPrefixMap.qualify(node)} cannot conform to abstract shape ${shape.showQualified(shapesPrefixMap)}"""
      }

     override def toJson: Json = Json.obj(
       ("type", Json.fromString("AbstractShapeErr"))
      ) 

  }

  case class AbstractShapeErrNoArgs() extends ShExError(s"Node cannot conform to abstract shape ") {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""AbstractShapeError cannot conform to abstract shape """
      }

     override def toJson: Json = Json.obj(
       ("type", Json.fromString("AbstractShapeErr"))
      ) 

  }



  case class NoDescendant(node: RDFNode, s:ShapeExpr, attempt: Attempt) extends ShExError(s"No descendant of shapeExpr ${s} matches node ${node.show}") {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""|No descendant of ${s.showQualified(shapesPrefixMap)} matches ${nodesPrefixMap.qualify(node)}
            |Attempt: ${attempt.showQualified(nodesPrefixMap,shapesPrefixMap)}
            |""".stripMargin
      }

     override def toJson: Json = Json.obj(
       ("type", Json.fromString("NoDescendantMatches")),
       ("attempt", attempt.asJson),
       ("node", Json.fromString(node.getLexicalForm)),
       ("shapeExpr", s.asJson)
      ) 

  }

  case class ExtendFails(node: RDFNode, extended:ShapeLabel, attempt: Attempt, err: ShExError)
    extends ShExError(
      s"""|ExtendFails: ${node.show} doesn't conform to extended shape ${extended.toRDFNode.show}
          |  Error obtained: ${err.msg}""".stripMargin) {
      override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
        s"""|Node ${nodesPrefixMap.qualify(node)} doesn't conform to extended shape ${shapesPrefixMap.qualify(extended.toRDFNode)}
            |Attempt: ${attempt.showQualified(nodesPrefixMap,shapesPrefixMap)}
            |Error: ${err.showQualified(nodesPrefixMap, shapesPrefixMap)}
            |""".stripMargin
      }

     override def toJson: Json = Json.obj(
       ("type", Json.fromString("NoDescendantMatches")),
       ("attempt", attempt.asJson),
       ("node", Json.fromString(node.getLexicalForm)),
       ("shape", Json.fromString(extended.toRDFNode.getLexicalForm)),
       ("error", err.asJson)
      ) 
  }

  case class NoPartition(
    node: RDFNode,
    attempt: Attempt, 
    s: Shape,
    extendedLabel: ShapeLabel,
    neighs: Neighs
    ) extends ShExError(s"""|No partition of neighs from node ${node.show} matches shape ${s.id.map(_.toRDFNode.show).getOrElse("")}. 
                            |Neighs = ${neighs} 
                            |Shape: ${extendedLabel.toRDFNode.show}""".stripMargin) {
    override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
      s"""|No partition of neighs from node ${nodesPrefixMap.qualify(node)} matches shape ${shapesPrefixMap.qualify(extendedLabel.toRDFNode)}
      |Available Neighs: ${neighs.showQualified(nodesPrefixMap)}
      |Shape: ${s.showQualified(shapesPrefixMap)}
      |Attempt: ${attempt.show}
      |""".stripMargin
    }

    override def toJson: Json = Json.obj(
       ("type", Json.fromString("NoPartition")),
       ("extended", Json.fromString(extendedLabel.toRDFNode.getLexicalForm)),
       ("shape", s.asJson),
//       ("neighs", neighs.asJson),
       ("attempt", attempt.asJson)
      ) 

  }