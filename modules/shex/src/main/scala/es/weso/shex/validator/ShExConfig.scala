package es.weso.shex.validator
import es.weso.rdf.RDFReader

case class ShExConfig(
  rdf: RDFReader,
  verbose: Boolean
)