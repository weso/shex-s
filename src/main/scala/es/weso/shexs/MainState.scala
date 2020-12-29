package es.weso.shexs

import es.weso.shapeMaps.ShapeMap
import es.weso.shex.Schema
import java.nio.file.Path
import java.nio.file.Paths

case class MainState(
  dataFormat: String,
  schema: Schema,
  schemaFormat: String,
  shapeMap: ShapeMap,
  shapeMapFormat: String,
  showDataFormat: String,
  showSchemaFormat: String,
  showResultFormat: String,
  folder: Path
)

object MainState {
  def initial: MainState =
    MainState(
      dataFormat = "Turtle",
      schema = Schema.empty, 
      schemaFormat = "ShExC", 
      shapeMap = ShapeMap.empty, 
      shapeMapFormat = "Compact", 
      showDataFormat = "Turtle",
      showSchemaFormat = "ShExC",
      showResultFormat = "JSON",
      folder = Paths.get(".")
    )
}