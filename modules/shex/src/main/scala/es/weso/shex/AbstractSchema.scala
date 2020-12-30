package es.weso.shex

//import java.nio.file.{Files, Paths}
//import cats.implicits._
//import es.weso.depgraphs.DepGraph
import es.weso.rdf._
import es.weso.rdf.nodes.{IRI, RDFNode}
//import es.weso.shex.shexR.{RDF2ShEx, ShEx2RDF}
//import es.weso.utils.UriUtils._
//import scala.io.Source
import scala.util._
import es.weso.rdf.locations.Location
//import cats.effect.IO

abstract class AbstractSchema { 
  def id: IRI
  def prefixes: Option[PrefixMap]
  def base: Option[IRI]
  def startActs: Option[List[SemAct]]
  def start: Option[ShapeExpr]
  def shapes: Option[List[ShapeExpr]]
  def optTripleExprMap: Option[Map[ShapeLabel,TripleExpr]]
  def imports: List[IRI]
  def labelLocationMap: Option[Map[ShapeLabel, Location]]

  
  def getTripleExpr(lbl: ShapeLabel): Either[String,TripleExpr] 
  def getShape(lbl: ShapeLabel): Either[String,ShapeExpr]

  lazy val prefixMap: PrefixMap = prefixes.getOrElse(PrefixMap.empty)

  lazy val shapesMap: Map[ShapeLabel,ShapeExpr] = {
    shapes match {
      case None => Map()
      case Some(ls) => {
        ls.collect{ case s if s.id.isDefined => (s.id.get, s)}.toMap
      }
    }
  }

  lazy val tripleExprMap: Map[ShapeLabel, TripleExpr] =
     optTripleExprMap.getOrElse(Map())

  def qualify(node: RDFNode): String =
    prefixMap.qualify(node)

  def qualify(label: ShapeLabel): String =
    prefixMap.qualify(label.toRDFNode)

  def labels: List[ShapeLabel] = shapesMap.keySet.toList

  override def toString: String = 
    shapesMap.
    map { // We don't show the label because it is already shown in se
      case (sl,se) => s"${se.showQualified(prefixMap)}" 
    }.
    mkString("\n")

}


