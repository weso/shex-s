package es.weso.shexs

import java.nio.file.Path
import es.weso.shextest.manifest.TestSelector
import com.monovore.decline.Opts
import cats.implicits._
import es.weso.shextest.manifest.ValidateManifest._
import cats.effect._
import es.weso.shextest.manifest.Result
import VerboseLevelOpt._
import es.weso.shextest.manifest._
import ValidatorVersion._
import es.weso.utils.VerboseLevel
import scala.concurrent.duration._
import es.weso.rdf.nodes.IRI
import java.nio.file.Paths
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.RDFReader
import es.weso.rdf.jena.Endpoint
import es.weso.shex.ResolvedSchema
import es.weso.rdf.PrefixMap
import es.weso.shapemaps.ShapeMap
import es.weso.utils.FileUtils._
import es.weso.shex.validator.Validator
import es.weso.shapemaps.ResultShapeMap
import es.weso.shex.validator.ExternalResolver

case class Validate(
    schemaSpec: SchemaSpec, 
    dataSpec: DataSpec, 
    shapeMapSpec: ShapeMapSpec, 
    validatorVersion: ValidatorVersion,
    showResultFormat: String, 
    output: Option[Path], 
    verbose: VerboseLevel)
 {

  def run(): IO[ExitCode] = for {
        res1 <- getRDFData(dataSpec, schemaSpec.baseIRI) // RDFAsJenaModel.fromURI(vc.data.toUri().toString(),vc.dataFormat,None)
        res2 <- RDFAsJenaModel.empty
        vv <- (res1,res2).tupled.use { 
      case (rdf,builder) => for {
       nodesPrefixMap <- rdf.getPrefixMap
       schema <- schemaSpec.getSchema(verbose)
       resolvedSchema <- ResolvedSchema.resolve(schema,None, verbose)
       shapeMap <- ShapeMapSpec.getShapeMapFromFile(shapeMapSpec.shapeMap, shapeMapSpec.shapeMapFormat,nodesPrefixMap, schema.prefixMap, schemaSpec.baseIRI)
       fixedMap <- ShapeMap.fixShapeMap(shapeMap, rdf, nodesPrefixMap, resolvedSchema.prefixMap)
       validator = validatorVersion.buildValidator(resolvedSchema, ExternalResolver.NoAction, builder)
       result   <- validator.validateShapeMap(rdf, fixedMap, verbose)
       resultShapeMap <- result.toResultShapeMap
       _             <- ShowResult.showResult(resultShapeMap, showResultFormat) 
      } yield ExitCode.Success}
    } yield vv

  private def getRDFData(dataSpec: DataSpec, baseIRI: Option[IRI]): IO[Resource[IO,RDFReader]] = dataSpec match {
     case DataPath(dataPath, dataFormat) => RDFAsJenaModel.fromURI(dataPath.toUri().toString(), dataFormat.getOrElse(DataFormat.defaultDataFormat), baseIRI)
     case EndpointOpt(uri) => IO(Resource.pure[IO,RDFReader](Endpoint(IRI(uri))))
   }

}

object Validate {

 lazy val validateCommand: Opts[Validate] = 
    Opts.subcommand("validate", "Validate RDF data using a schema and a shape map") {
      (SchemaSpec.schemaSpec, DataSpec.dataSpec, ShapeMapSpec.shapeMapSpec, ValidatorVersion.validatorVersion, ShowResult.showResultFormatOpt, OutputOpt.outputOpt, VerboseLevelOpt.verboseLevel)
      .mapN(Validate.apply)
    }
    
}
