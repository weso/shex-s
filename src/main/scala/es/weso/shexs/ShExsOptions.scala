package es.weso.shexs

import es.weso.rdf.nodes._
import es.weso.utils.VerboseLevel

case class ShExsOptions(
    verbose: VerboseLevel,
    dataFormat: String,
    schemaFormat: String,
    shapemapFormat: String,
    base: Option[IRI]
)

object ShExsOptions {
  def defaultOptions(): ShExsOptions =
    new ShExsOptions(
      verbose = VerboseLevel.Nothing,
      dataFormat = "TURTLE",
      schemaFormat = "ShExC",
      shapemapFormat = "Compact",
      base = None
    )
}
