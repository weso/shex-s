package es.weso.shexs

import java.nio.file.Path
import com.monovore.decline.Opts
import cats.implicits._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
import cats.effect.IO
import es.weso.utils.FileUtils._
import es.weso.shapemaps.ShapeMap

case class ShapeMapSpec(shapeMap: Path, shapeMapFormat: String)

object ShapeMapSpec {

  lazy val availableShapeMapFormats    = List("Compact", "JSON")
  lazy val defaultShapeMapFormat       = availableShapeMapFormats.head
  lazy val availableShapeMapFormatsStr = availableShapeMapFormats.mkString(",")

  lazy val shapeMapOpt = Opts.option[Path]("shapeMap", short = "sm", help = "Path to shapeMap file.")
  lazy val shapeMapFormatOpt = Opts
    .option[String](
      "shapeMapFormat",
      help = s"ShapeMap format, default=$defaultShapeMapFormat, available formats=$availableShapeMapFormats"
    )
    .withDefault(defaultShapeMapFormat)

  lazy val shapeMapSpec = (shapeMapOpt, shapeMapFormatOpt).mapN(ShapeMapSpec.apply)

  def getShapeMapFromFile(
      filePath: Path,
      shapeMapFormat: String,
      nodesPrefixMap: PrefixMap,
      shapesPrefixMap: PrefixMap,
      baseIRI: Option[IRI]
  ): IO[ShapeMap] =
    for {
      str <- getContents(filePath).handleErrorWith(e =>
        IO.raiseError(
          new RuntimeException(
            s"Error obtaining shapeMap from file: ${filePath.toFile().getAbsolutePath()} with format ${shapeMapFormat}: ${e.getMessage()}"
          )
        )
      )
      sm <- IO.fromEither(
        ShapeMap
          .fromString(str.toString, shapeMapFormat, baseIRI, nodesPrefixMap, shapesPrefixMap)
          .leftMap(err => new RuntimeException(s"Error parsing shapeMap: ${err})"))
      )
    } yield sm

}
