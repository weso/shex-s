package es.weso.shapemaps

import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.rdf.path.SHACLPath
import io.circe._
import io.circe.syntax._
import fs2.Stream
import cats.effect.IO
import es.weso.utils.json.DecoderUtils._
import es.weso.rdf.{PrefixMap, RDFReader}


abstract class NodeSelector {
  def select(rdf: RDFReader): Stream[IO,RDFNode]

  def relativize(base: IRI): NodeSelector
}

case class RDFNodeSelector(node: RDFNode) extends NodeSelector {
  override def select(rdf: RDFReader): Stream[IO, RDFNode] =
    Stream.emit(node)

  override def relativize(base: IRI): NodeSelector =
    RDFNodeSelector(node.relativize(base))


}

case class TriplePattern(
  subjectPattern: Pattern,
  path: SHACLPath,
  objectPattern: Pattern) extends NodeSelector {
  override def select(rdf: RDFReader): Stream[IO,RDFNode] =
    (subjectPattern, path, objectPattern) match {
      case (Focus,p,WildCard) => rdf.nodesWithPath(p).map(_._1)
      case (Focus,p,NodePattern(obj)) => rdf.subjectsWithPath(p,obj)
      case (WildCard,p,Focus) => rdf.nodesWithPath(p).map(_._2)
      case (NodePattern(subj),p,Focus) =>  rdf.objectsWithPath(subj, p)
      case _ => Stream.raiseError[IO](new Exception(s"Strange triple pattern in node selector: $this"))
    }

  override def relativize(base: IRI): NodeSelector =
    TriplePattern(subjectPattern.relativize(base),
      path.relativize(base),
      objectPattern.relativize(base))

}

case class SparqlSelector(query: String) extends NodeSelector {
  override def select(rdf: RDFReader): Stream[IO,RDFNode] = {
    def map2Ls(m : Map[String,RDFNode]): RDFNode = if (m.size == 1) {
      m.map(_._2).head
    } else 
      throw new RuntimeException(s"Result of query has more than one value: $m")
    rdf.querySelect(query).map(map2Ls)
  }

  override def relativize(base: IRI): SparqlSelector = this

}

case class GenericSelector(iri: IRI, param: String) extends NodeSelector {
  override def select(rdf: RDFReader): Stream[IO,RDFNode] = {
    Stream.raiseError[IO](new RuntimeException(s"Not implemented GenericSelector($iri, $param)"))
  }

  override def relativize(base: IRI) = this

}

object NodeSelector {

  def fromString(str: String, base: Option[String], pm: PrefixMap): Either[String,NodeSelector] =
    ParserNodeSelector.parse(str, base, pm)

  implicit val encodeNodeSelector: Encoder[NodeSelector] = new Encoder[NodeSelector] {
    final def apply(nodeSelector: NodeSelector): Json = {
      nodeSelector match {
        case RDFNodeSelector(node) => Json.fromString(node.toString)
        case TriplePattern(subj, path, obj) => {
          Json.fromJsonObject(JsonObject.empty.
            add("subject", subj.asJson).
            add("path", path.asJson).
            add("object", obj.asJson))
        }
        case SparqlSelector(query) =>
          Json.fromJsonObject(JsonObject.empty.add("sparql", Json.fromString(query)))
      }
    }
  }

  implicit lazy val decodeRDFNodeSelector: Decoder[RDFNodeSelector] = Decoder.instance { c => {
    c.as[String].flatMap(s => {
      RDFNode.fromString(s).fold(
      s => {
        Left(DecodingFailure(s, Nil))
      } ,
      node => Right(RDFNodeSelector(node)))
    })
    }
  }

  implicit lazy val decodeTriplePattern: Decoder[TriplePattern] = Decoder.instance { c =>
    for {
      subj <- fieldDecode[Pattern](c, "subject")
      path <- fieldDecode[SHACLPath](c, "path")
      obj <- fieldDecode[Pattern](c, "object")
    } yield TriplePattern(subj, path, obj)

  }

  implicit lazy val decodeSparql: Decoder[SparqlSelector] = Decoder.instance { c =>
    for {
      query <- fieldDecode[String](c, "sparql")
    } yield SparqlSelector(query)
  }

  implicit lazy val decodeNodeSelector: Decoder[NodeSelector] =
    Decoder[RDFNodeSelector].map(identity).or(
     Decoder[TriplePattern].map(identity).or(
      Decoder[SparqlSelector].map(identity)
    )
   )

}