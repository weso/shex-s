package es.weso.shexs
import cats.effect._
import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shapemaps.ShapeMap
import es.weso.shex.{ResolvedSchema, Schema}
import es.weso.shex.validator.Validator
import es.weso.shextest.manifest._
import com.monovore.decline._
import com.monovore.decline.effect._
import buildinfo._
import java.nio.file.Path
import es.weso.shapepath.schemamappings.SchemaMappings
import es.weso.shex.implicits.showShEx._
import es.weso.shapepath.ProcessingError
import es.weso.shapepath.ShapePath
import es.weso.shapemaps.ResultShapeMap
import java.net.URI
import scala.util.Try
import cats.data.Validated
import es.weso.rdf.jena.Endpoint
import es.weso.rdf.nodes.IRI
import es.weso.wikibaserdf.WikibaseRDF
import es.weso.utils.FileUtils._
import es.weso.utils.VerboseLevel

// Commands  
case class SchemaMapping(
    schemaSpec: SchemaSpec, 
    mapping: Path, 
    output: Option[Path], 
    verbose: VerboseLevel)
case class Validate(
    schemaSpec: SchemaSpec, 
    dataSpec: DataSpec, 
    shapeMapSpec: ShapeMapSpec, 
    showResultFormat: String, 
    output: Option[Path], 
    verbose: VerboseLevel)
case class WikibaseValidate(
    schemaSpec: SchemaSpec, 
    endpoint: EndpointOpt, 
    prefixMapPath: Option[Path], 
    shapeMapSpec: ShapeMapSpec, 
    showResultFormat: String, 
    output: Option[Path], 
    verbose: VerboseLevel)

case class ShapePathEval(
    schemaSpec: SchemaSpec, 
    shapePath: String, 
    output: Option[Path], 
    verbose: VerboseLevel)

  
  
sealed abstract class DataSpec
case class DataPath(dataPath: Path, dataFormat: Option[String]) extends DataSpec
case class EndpointOpt(uri: URI) extends DataSpec

case class ShapeMapSpec(
    shapeMap: Path, 
    shapeMapFormat: String)

object Main extends CommandIOApp (
  name="shex-s", 
  header = "ShEx-Scala command line tool",
  version = BuildInfo.version
  ) {
 

lazy val availableDataFormats = List("Turtle", "NTriples","RDF/XML","JSON-LD")
lazy val defaultDataFormat = availableDataFormats.head
lazy val availableDataFormatsStr = availableDataFormats.mkString(",")

lazy val availableShapeMapFormats = List("Compact", "JSON")
lazy val defaultShapeMapFormat = availableShapeMapFormats.head
lazy val availableShapeMapFormatsStr = availableShapeMapFormats.mkString(",")

lazy val outputOpt = Opts.option[Path]("output","Output to file (default = console)").orNone
lazy val mappingOpt = Opts.option[Path]("mapping", short = "m", metavar = "mappings-file", help = "Path to Mappings file.")
lazy val dataOpt = Opts.option[Path]("data", short = "d", help = "Path to data file.")
  
lazy val dataFormatOpt = Opts.option[String]("dataFormat", help = s"Data format. Default=$defaultDataFormat, available=$availableDataFormatsStr").withDefault(defaultDataFormat)
lazy val shapeMapOpt = Opts.option[Path]("shapeMap", short = "sm", help = "Path to shapeMap file.")
lazy val shapeMapFormatOpt = Opts.option[String]("shapeMapFormat", help = s"ShapeMap format, default=$defaultShapeMapFormat, available formats=$availableShapeMapFormats").withDefault(defaultShapeMapFormat)
lazy val shapePathOpt = Opts.option[String]("shapePath", help = s"ShapePath to validate a schema")
lazy val showResultFormatOpt = Opts.option[String]("showResultFormat", help = s"showResultFormat").withDefault("details")

lazy val dataPath: Opts[DataPath] = (dataOpt,dataFormatOpt).mapN {
    case (path,format) => DataPath(path,Some(format))
  }


lazy val endpoint: Opts[EndpointOpt] = UriOpt.uri("endpoint", "endpoint URL").map(EndpointOpt.apply)

  
lazy val dataSpec: Opts[DataSpec] = dataPath orElse endpoint

lazy val shapeMapSpec = (shapeMapOpt, shapeMapFormatOpt).mapN(ShapeMapSpec.apply)
  
lazy val schemaMappingCommand: Opts[SchemaMapping] = 
    Opts.subcommand("mapping", "Convert a schema through a mapping") {
      (SchemaSpec.schemaSpec, mappingOpt, outputOpt, VerboseLevelOpt.verboseLevel).mapN(SchemaMapping.apply)
    }

lazy val validateCommand: Opts[Validate] = 
    Opts.subcommand("validate", "Validate RDF data using a schema and a shape map") {
      (SchemaSpec.schemaSpec, dataSpec, shapeMapSpec, showResultFormatOpt, outputOpt, VerboseLevelOpt.verboseLevel)
      .mapN(Validate.apply)
    }

lazy val shapePathValidateCommand: Opts[ShapePathEval] =
    Opts.subcommand("shapePath","Validate a shape path") {
      (SchemaSpec.schemaSpec, shapePathOpt, outputOpt, VerboseLevelOpt.verboseLevel)
      .mapN(ShapePathEval.apply)
    }

lazy val prefixMapPath: Opts[Option[Path]] = Opts.option[Path]("prefixMapPath","path containing prefix map declarations (Wikidata by default)").orNone

lazy val wikibaseCommand: Opts[WikibaseValidate] = 
    Opts.subcommand("wikibase", "Validate RDF data from wikibase") {
      (SchemaSpec.schemaSpec, endpoint, prefixMapPath, shapeMapSpec, showResultFormatOpt, outputOpt, VerboseLevelOpt.verboseLevel)
      .mapN(WikibaseValidate.apply)
    }

def info(msg: String, verbose: Boolean): IO[Unit] = 
   if (verbose) IO.println(msg)
   else IO(())

override def main: Opts[IO[ExitCode]] =
   (schemaMappingCommand orElse 
    validateCommand orElse
    shapePathValidateCommand orElse 
    Manifest.manifestCommand orElse
    wikibaseCommand orElse 
    SchemaCommand.schemaCommand
   ).map {
     case smc: SchemaMapping => doSchemaMapping(smc) 
     case vc : Validate => doValidate(vc)
     case spc: ShapePathEval => doShapePathEval(spc)
     case mf: Manifest => mf.run()
     case wc: WikibaseValidate => doWikibaseValidate(wc)
     case sc: SchemaCommand => sc.run()
   }.map(
     _.handleErrorWith(infoError)
   ) 

def infoError(err: Throwable): IO[ExitCode] =
    IO.println(s"Error ${err.getLocalizedMessage()}") *> IO(ExitCode.Error)

  

def doSchemaMapping(smc: SchemaMapping): IO[ExitCode] = for {
       schema <- smc.schemaSpec.getSchema(smc.verbose)
       mappingStr <- getContents(smc.mapping)
       mapping <- IO.fromEither(SchemaMappings
        .fromString(mappingStr.toString)
        .leftMap(err => new RuntimeException(s"Error parsing schema mappings: ${err}"))
        )
       newSchema <- mapping.convert(schema).fold(
         err => IO.raiseError(new RuntimeException(err.map(_.toString).mkString("\n"))),
         s => s.pure[IO],
         (warnings: List[ProcessingError], s: Schema) => for {
           _ <- IO.println(warnings.map(_.toString).mkString("\n"))
         } yield s
       )
       _ <- smc.output match {
         case None => IO.println(newSchema.show)
         case Some(outputPath) => for { 
           _ <- writeFile(outputPath.toFile().getAbsolutePath(), newSchema.show)
           _ <- IO.println(s"Output saved in ${outputPath}")
         } yield ()  
       } 
     } yield ExitCode.Success


def getRDFData(dataSpec: DataSpec, baseIRI: Option[IRI]): IO[Resource[IO,RDFReader]] = dataSpec match {
     case DataPath(dataPath, dataFormat) => RDFAsJenaModel.fromURI(dataPath.toUri().toString(), dataFormat.getOrElse(defaultDataFormat), baseIRI)
     case EndpointOpt(uri) => IO(Resource.pure[IO,RDFReader](Endpoint(IRI(uri))))
   }
   

def doValidate(vc: Validate): IO[ExitCode] = 
    for {
        res1 <- getRDFData(vc.dataSpec, vc.schemaSpec.baseIRI) // RDFAsJenaModel.fromURI(vc.data.toUri().toString(),vc.dataFormat,None)
        res2 <- RDFAsJenaModel.empty
        vv <- (res1,res2).tupled.use { 
      case (rdf,builder) => for {
       nodesPrefixMap <- rdf.getPrefixMap
       schema <- vc.schemaSpec.getSchema(vc.verbose)
       resolvedSchema <- ResolvedSchema.resolve(schema,None, vc.verbose)
       shapeMap <- getShapeMapFromFile(vc.shapeMapSpec.shapeMap,vc.shapeMapSpec.shapeMapFormat,nodesPrefixMap, schema.prefixMap, vc.schemaSpec.baseIRI)
       fixedMap <- ShapeMap.fixShapeMap(shapeMap, rdf, nodesPrefixMap, resolvedSchema.prefixMap)
       result   <- Validator.validate(resolvedSchema, fixedMap, rdf, builder, vc.verbose)
       resultShapeMap <- result.toResultShapeMap
       _             <- showResult(resultShapeMap, vc.showResultFormat) 
      } yield ExitCode.Success}
    } yield vv

def doWikibaseValidate(wc: WikibaseValidate): IO[ExitCode] = 
    getPrefixMap(wc.prefixMapPath).flatMap(pm => 
    for {
        res1 <- getWikibaseRDF(wc.endpoint, pm) 
        res2 <- RDFAsJenaModel.empty
        vv <- (res1,res2).tupled.use { 
      case (rdf,builder) => for {
       nodesPrefixMap <- rdf.getPrefixMap
       schema <- wc.schemaSpec.getSchema(wc.verbose)
       resolvedSchema <- ResolvedSchema.resolve(schema,None, wc.verbose)
       shapeMap <- getShapeMapFromFile(wc.shapeMapSpec.shapeMap,wc.shapeMapSpec.shapeMapFormat,nodesPrefixMap, schema.prefixMap, wc.schemaSpec.baseIRI)
       fixedMap <- ShapeMap.fixShapeMap(shapeMap, rdf, nodesPrefixMap, resolvedSchema.prefixMap)
       result   <- Validator.validate(resolvedSchema, fixedMap, rdf, builder, wc.verbose)
       resultShapeMap <- result.toResultShapeMap
       _             <- showResult(resultShapeMap, wc.showResultFormat) 
      } yield ExitCode.Success}
    } yield vv)

def getPrefixMap(maybePath: Option[Path]): IO[PrefixMap] = maybePath match {
    case None => IO(WikibaseRDF.wikidataPrefixMap)
    case Some(path) => {
      RDFAsJenaModel.fromURI(path.toUri().toString(), "Turtle")
      .flatMap(_.use(rdf => rdf.getPrefixMap))
    }
  }


def getWikibaseRDF(ep: EndpointOpt, pm: PrefixMap): IO[Resource[IO,WikibaseRDF]] = 
   WikibaseRDF.fromEndpoint(IRI(ep.uri), pm)

def doShapePathEval(spc: ShapePathEval): IO[ExitCode] = for {
     schema <- spc.schemaSpec.getSchema(spc.verbose)
     shapePath <- IO.fromEither(ShapePath.fromString(spc.shapePath, "Compact", None, schema.prefixMap).leftMap(err => new RuntimeException(s"Error parsing shapePath: ${err}")))
     result <- { 
       val (ls,v) = ShapePath.eval(shapePath,schema)
       IO.println(ls.map(_.toString).mkString("\n")) *>
       v.pure[IO]
     } 
   } yield ExitCode.Success 

def showResult(result: ResultShapeMap, showResultFormat: String): IO[Unit] =
    IO.println(result.serialize(showResultFormat).fold(
      err => s"Error serializing ${result} with format ${showResultFormat}: $err", 
      identity)
    )


def getShapeMapFromFile(filePath: Path, 
                                  shapeMapFormat: String,
                                  nodesPrefixMap: PrefixMap,
                                  shapesPrefixMap: PrefixMap,
                                  baseIRI: Option[IRI]
                                  ): IO[ShapeMap] =
    for {
      str <- getContents(filePath).handleErrorWith(e => IO.raiseError(new RuntimeException(s"Error obtaining shapeMap from file: ${filePath.toFile().getAbsolutePath()} with format ${shapeMapFormat}: ${e.getMessage()}")))
      sm <- IO.fromEither(ShapeMap.fromString(str.toString, shapeMapFormat, baseIRI, nodesPrefixMap,shapesPrefixMap)
        .leftMap(err => new RuntimeException(s"Error parsing shapeMap: ${err})")))
    } yield sm

}
