package es.weso.shexs

import cats.effect._
import es.weso.rdf.RDFReader
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shapeMaps.ShapeMap
import es.weso.shex.Schema

case class MainState(
  data: Resource[IO,RDFReader],
  dataFormat: String,
  schema: Schema,
  schemaFormat: String,
  shapeMap: ShapeMap,
  shapeMapFormat: String,
  showDataFormat: String
)

object MainState {
  def initial: IO[MainState] =
    IO(MainState(RDFAsJenaModel.empty,"Turtle",Schema.empty, "ShExC", ShapeMap.empty, "Compact", "Turtle"))
}