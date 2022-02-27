package es.weso.shex.validator

import cats._
import cats.implicits._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import io.circe._
import io.circe.syntax._
import es.weso.rdf.RDFReader
import es.weso.rdf.locations.Location
import io.circe.generic.auto._, io.circe.syntax._

// import es.weso.shex.implicits.encoderShEx.encodeShape

/**
 * Represents current validation attempt
 * It contains the node and a shape
 * It may contain a predicate, path or nothing
 */
case class Attempt(
  nodeShape: NodeShape, 
  path: Option[IRI],
  rdf: RDFReader  
) {
  def node = nodeShape.node
  def shape = nodeShape.st

  def predicate: Option[IRI] = path

  def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
    nodeShape.showQualified(nodesPrefixMap,shapesPrefixMap) ++ path.map(iri => s" Path: ${shapesPrefixMap.qualifyIRI(iri)}").getOrElse("")
  }

  override def toString: String = Attempt.showAttempt.show(this)
}

object Attempt {
  
  implicit def showAttempt: Show[Attempt] = new Show[Attempt] {
    import NodeShape._
    override def show(t: Attempt): String = {
      s"Attempt: ${t.nodeShape.show}\npath: ${t.path.map(_.str).getOrElse("")}"
    }
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


  implicit val attemptEncoder: Encoder[Attempt] = new Encoder[Attempt] {
    
    final def apply(attempt: Attempt): Json = {
      val locations = attempt.rdf.nodeLocations.get(attempt.node)
      Json.fromFields(
        List(
         ("node", attempt.nodeShape.node.getLexicalForm.asJson),
         ("shape", attempt.nodeShape.st.asJson)
        ) ++ (if (locations.isEmpty) List()
              else List(("location", locations.toList.asJson)))
      ) 
    }
  }


}

