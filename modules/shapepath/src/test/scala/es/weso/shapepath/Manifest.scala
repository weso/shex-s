package es.weso.shapepath
import io.circe._
import io.circe.generic.semiauto._

case class ManifestEntry(
    name: String,
    from: String,
    shapePath: String,
    expect: String,
    throws: Option[Boolean],
    status: Option[String],
    comment: Option[String]
)

case class Manifest(
 description: String,
 tests: List[ManifestEntry]
)

object Manifest {
  implicit val entryDecoder: Decoder[ManifestEntry] = deriveDecoder
  implicit val entryEncoder: Encoder[ManifestEntry] = deriveEncoder
  implicit val manifestDecoder: Decoder[Manifest] = deriveDecoder
  implicit val manifestEncoder: Encoder[Manifest] = deriveEncoder
}

