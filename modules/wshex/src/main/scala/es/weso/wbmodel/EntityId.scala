package es.weso.wbmodel
import es.weso.rdf.nodes._
import cats.implicits._

abstract class EntityId extends Value {
  def id: String
  def iri: IRI
}

object EntityId {

  def fromIri(iri: IRI): Either[String, EntityId] = {
    val (name, base) = Utils.splitIri(iri)
    name(0) match {
      case 'P' => PropertyId(name, iri).asRight
      case 'Q' => ItemId(name, iri).asRight
      case _ =>
        s"""|Match error. EntityId.fromIri($iri):
            | localName: $name
            | base: $base
            | Should start by P or Q
            |""".stripMargin.asLeft
    }
  }
}
