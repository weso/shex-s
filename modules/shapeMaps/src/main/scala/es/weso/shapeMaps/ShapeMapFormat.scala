package es.weso.shapeMaps
import cats.implicits._

sealed abstract class ShapeMapFormat {
    def name: String 
}

object ShapeMapFormat {
    def fromString(name: String): Either[String,ShapeMapFormat] = 
      availableFormats.
      filter(_.name.toLowerCase == name.toLowerCase).
      headOption match {
          case None => s"Not found shape format $name\nAvailable formats: ${availableFormatNames}".asLeft[ShapeMapFormat]
          case Some(smf) => smf.asRight[String]
      }

    def availableFormats: List[ShapeMapFormat] = List(Compact,JsonShapeMapFormat)
    def availableFormatNames: List[String] = availableFormats.map(_.name)
}

case object Compact extends ShapeMapFormat {
    def name = "compact"
}

case object JsonShapeMapFormat extends ShapeMapFormat {
    def name = "json"
}
