package es.weso.shexs

import java.nio.file.Path
import java.net.URI
import com.monovore.decline.Opts
import cats.implicits._
import es.weso.rdf.nodes.IRI
import cats.effect.IO
import es.weso.shex.Schema
import es.weso.utils.VerboseLevel

sealed abstract class SchemaSpec {
  def getSchema(verbose: VerboseLevel): IO[Schema]
  val baseIRI: Option[IRI]
}

case class SchemaPath(
    schema: Path,
    schemaFormat: String,
    baseIRI: Option[IRI]
) extends SchemaSpec {
  override def getSchema(verbose: VerboseLevel): IO[Schema] =
    Schema.fromFile(schema.toFile().getAbsolutePath(), schemaFormat, baseIRI, None)
}

case class SchemaURI(
    uri: URI,
    baseIRI: Option[IRI]
) extends SchemaSpec {
  override def getSchema(verbose: VerboseLevel): IO[Schema] = Schema.fromIRI(IRI(uri), baseIRI, verbose)
}

object SchemaSpec {

  lazy val availableSchemaFormats    = List("ShExC", "ShExJ")
  lazy val defaultSchemaFormat       = availableSchemaFormats.head
  lazy val availableSchemaFormatsStr = availableSchemaFormats.mkString(",")

  lazy val schemaOpt = Opts.option[Path]("schema", short = "s", help = "Path to ShEx file.")
  lazy val schemaFormatOpt = Opts
    .option[String](
      "schemaFormat",
      metavar = "format",
      help = s"Schema format, default = ($defaultSchemaFormat). Possible values = ($availableSchemaFormatsStr)"
    )
    .withDefault(defaultSchemaFormat)

  lazy val baseIRI: Opts[Option[IRI]] =
    (UriOpt.uri(s"baseIRI", "base IRI")).orNone.map {
      case Some(uri) => Some(IRI(uri))
      case None      => Some(IRI(s"http://base/"))
    }

  lazy val schemaURI: Opts[SchemaURI] =
    (UriOpt.uri("schemaURL", "URL of schema"), baseIRI).mapN(SchemaURI.apply)

  lazy val schemaPath: Opts[SchemaPath] =
    (schemaOpt, schemaFormatOpt, baseIRI).mapN(SchemaPath.apply)

  lazy val schemaSpec: Opts[SchemaSpec] = schemaPath orElse schemaURI

}
