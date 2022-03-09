package es.weso.shex

import es.weso.rdf.nodes.IRI

case class Annotation(predicate: IRI, obj: ObjectValue) {
  def relativize(base: IRI): Annotation =
    Annotation(predicate.relativizeIRI(base),obj.relativize(base))
}
