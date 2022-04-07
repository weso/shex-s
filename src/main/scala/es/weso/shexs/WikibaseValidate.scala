package es.weso.shexs

import java.nio.file.Path
import es.weso.utils.VerboseLevel
import com.monovore.decline.Opts
import cats.effect._
import es.weso.wikibaserdf.WikibaseRDF
import es.weso.rdf.PrefixMap
import es.weso.rdf.jena.RDFAsJenaModel
import cats.implicits._
import cats.effect.ExitCode
import es.weso.shex.ResolvedSchema
import es.weso.rdf.nodes.IRI
import es.weso.shapemaps.ShapeMap
import es.weso.shex.validator.Validator

case class WikibaseValidate(
    schemaSpec: SchemaSpec,
    endpoint: EndpointOpt,
    prefixMapPath: Option[Path],
    shapeMapSpec: ShapeMapSpec,
    showResultFormat: String,
    output: Option[Path],
    verbose: VerboseLevel
) {

  def run(): IO[ExitCode] =
    getPrefixMap(prefixMapPath).flatMap(pm =>
      for {
        res1 <- getWikibaseRDF(endpoint, pm)
        res2 <- RDFAsJenaModel.empty
        vv <- (res1, res2).tupled.use { case (rdf, builder) =>
          for {
            nodesPrefixMap <- rdf.getPrefixMap
            schema         <- schemaSpec.getSchema(verbose)
            resolvedSchema <- ResolvedSchema.resolve(schema, None, verbose)
            shapeMap <- ShapeMapSpec.getShapeMapFromFile(
              shapeMapSpec.shapeMap,
              shapeMapSpec.shapeMapFormat,
              nodesPrefixMap,
              schema.prefixMap,
              schemaSpec.baseIRI
            )
            fixedMap       <- ShapeMap.fixShapeMap(shapeMap, rdf, nodesPrefixMap, resolvedSchema.prefixMap)
            result         <- Validator.validate(resolvedSchema, fixedMap, rdf, builder, verbose)
            resultShapeMap <- result.toResultShapeMap
            _              <- ShowResult.showResult(resultShapeMap, showResultFormat)
          } yield ExitCode.Success
        }
      } yield vv
    )

  private def getPrefixMap(maybePath: Option[Path]): IO[PrefixMap] = maybePath match {
    case None => IO(WikibaseRDF.wikidataPrefixMap)
    case Some(path) => {
      RDFAsJenaModel
        .fromURI(path.toUri().toString(), "Turtle")
        .flatMap(_.use(rdf => rdf.getPrefixMap))
    }
  }

  private def getWikibaseRDF(ep: EndpointOpt, pm: PrefixMap): IO[Resource[IO, WikibaseRDF]] =
    WikibaseRDF.fromEndpoint(IRI(ep.uri), pm)

}

object WikibaseValidate {

  lazy val prefixMapPath: Opts[Option[Path]] =
    Opts.option[Path]("prefixMapPath", "path containing prefix map declarations (Wikidata by default)").orNone

  lazy val wikibaseCommand: Opts[WikibaseValidate] =
    Opts.subcommand("wikibase", "Validate RDF data from wikibase") {
      (
        SchemaSpec.schemaSpec,
        EndpointOpt.endpoint,
        prefixMapPath,
        ShapeMapSpec.shapeMapSpec,
        ShowResult.showResultFormatOpt,
        OutputOpt.outputOpt,
        VerboseLevelOpt.verboseLevel
      )
        .mapN(WikibaseValidate.apply)
    }
}
