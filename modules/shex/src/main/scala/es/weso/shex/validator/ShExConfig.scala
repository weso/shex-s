package es.weso.shex.validator
import es.weso.rdf.RDFReader
import es.weso.utils.VerboseLevel

case class ShExConfig(
  rdf: RDFReader,
  verboseLevel: VerboseLevel
)