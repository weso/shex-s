package es.weso.shexs
// import java.nio.file.Paths
// import cats.arrow.FunctionK
// import cats.data.StateT
import cats.effect._
import cats.implicits._
// import cats.~>
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shapemaps.ShapeMap
import es.weso.shex.{ResolvedSchema, Schema}
import es.weso.shex.validator.Validator
//import org.rogach.scallop._
//import org.rogach.scallop.exceptions._
import es.weso.shextest.manifest._
// import es.weso.shextest.manifest.ShExManifest
// import fs2._
//import es.weso.shex.validator.ValidationLog
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
// import es.weso.utils.IOException
import es.weso.rdf.jena.Endpoint
import es.weso.rdf.nodes.IRI
import es.weso.wikibaserdf.WikibaseRDF
import es.weso.utils.FileUtils._

object Main extends CommandIOApp (
  name="shex-s", 
  header = "ShEx-Scala command line tool",
  version = BuildInfo.version
  ) {


  // Commands  
  case class SchemaMapping(
    schemaSpec: SchemaSpec, 
    mapping: Path, 
    baseIRI: Option[IRI],
    output: Option[Path], 
    verbose: Boolean)
  case class Validate(
    schemaSpec: SchemaSpec, 
    dataSpec: DataSpec, 
    shapeMapSpec: ShapeMapSpec, 
    baseIRI: Option[IRI], 
    showResultFormat: String, 
    output: Option[Path], 
    verbose: Boolean)
  case class WikibaseValidate(
    schemaSpec: SchemaSpec, 
    endpoint: EndpointOpt, 
    prefixMapPath: Option[Path], 
    shapeMapSpec: ShapeMapSpec, 
    baseIRI: Option[IRI],
    showResultFormat: String, 
    output: Option[Path], verbose: Boolean)
  case class ShapePathEval(
    schemaSpec: SchemaSpec, 
    shapePath: String, 
    baseIRI: Option[IRI],
    output: Option[Path], 
    verbose: Boolean)
  case class Manifest(
    manifestPath: Path, 
    verbose: Boolean
  )
  
  sealed abstract class SchemaSpec
  case class SchemaPath(schema: Path, schemaFormat: String) extends SchemaSpec
  case class SchemaURI(uri: URI) extends SchemaSpec
  
  sealed abstract class DataSpec
  case class DataPath(dataPath: Path, dataFormat: Option[String]) extends DataSpec
  case class EndpointOpt(uri: URI) extends DataSpec

  case class ShapeMapSpec(
    shapeMap: Path, 
    shapeMapFormat: String)

  lazy val availableSchemaFormats = List("ShExC", "ShExJ")
  lazy val defaultSchemaFormat = availableSchemaFormats.head
  lazy val availableSchemaFormatsStr = availableSchemaFormats.mkString(",")

  lazy val availableDataFormats = List("Turtle", "NTriples","RDF/XML","JSON-LD")
  lazy val defaultDataFormat = availableDataFormats.head
  lazy val availableDataFormatsStr = availableDataFormats.mkString(",")

  lazy val availableShapeMapFormats = List("Compact", "JSON")
  lazy val defaultShapeMapFormat = availableShapeMapFormats.head
  lazy val availableShapeMapFormatsStr = availableShapeMapFormats.mkString(",")

  lazy val schemaOpt = Opts.option[Path]("schema", short = "s", help = "Path to ShEx file.")
  lazy val schemaFormatOpt = Opts.option[String]("schemaFormat", metavar = "format", help = s"Schema format, default = ($defaultSchemaFormat). Possible values = ($availableSchemaFormatsStr)").withDefault(defaultSchemaFormat)
  lazy val outputOpt = Opts.option[Path]("output","Output to file (default = console)").orNone
  lazy val verboseOpt = Opts.flag("verbose", "show extra information").orFalse
  lazy val mappingOpt = Opts.option[Path]("mapping", short = "m", metavar = "mappings-file", help = "Path to Mappings file.")
  lazy val dataOpt = Opts.option[Path]("data", short = "d", help = "Path to data file.")
  
  lazy val dataFormatOpt = Opts.option[String]("dataFormat", help = s"Data format. Default=$defaultDataFormat, available=$availableDataFormatsStr").withDefault(defaultDataFormat)
  lazy val shapeMapOpt = Opts.option[Path]("shapeMap", short = "sm", help = "Path to shapeMap file.")
  lazy val shapeMapFormatOpt = Opts.option[String]("shapeMapFormat", help = s"ShapeMap format, default=$defaultShapeMapFormat, available formats=$availableShapeMapFormats").withDefault(defaultShapeMapFormat)
  lazy val shapePathOpt = Opts.option[String]("shapePath", help = s"ShapePath to validate a schema")
  lazy val showResultFormatOpt = Opts.option[String]("showResultFormat", help = s"showResultFormat").withDefault("details")
  lazy val schemaPath: Opts[SchemaPath] = 
    (schemaOpt, schemaFormatOpt).mapN { case (path, format) => SchemaPath(path, format)}

  lazy val dataPath: Opts[DataPath] = (dataOpt,dataFormatOpt).mapN {
    case (path,format) => DataPath(path,Some(format))
  }
    
  lazy val endpoint: Opts[EndpointOpt] = uri("endpoint", "endpoint URL").map(EndpointOpt)

  def uri(name: String, helpStr: String): Opts[URI] = 
    Opts.option[String](name, help =helpStr).mapValidated(s => 
      Try(new URI(s)).fold(
        exc => Validated.invalidNel(s"Error converting to URL: ${exc.getMessage}"), 
        url => Validated.valid(url))
    )
  
  lazy val schemaURI: Opts[SchemaURI] = 
    (uri("schemaURL", "URL of schema")).map(SchemaURI)

  lazy val baseIRI: Opts[Option[IRI]] =
    (uri(s"baseIRI", "base IRI")).orNone.map {
      case Some(uri) => Some(IRI(uri))
      case None => Some(IRI(s"http://base/"))
    }

  lazy val schemaSpec: Opts[SchemaSpec] = schemaPath orElse schemaURI
  lazy val dataSpec: Opts[DataSpec] = dataPath orElse endpoint

  lazy val shapeMapSpec = (shapeMapOpt, shapeMapFormatOpt).mapN(ShapeMapSpec)
  
  lazy val schemaMappingCommand: Opts[SchemaMapping] = 
    Opts.subcommand("mapping", "Convert a schema through a mapping") {
      (schemaSpec, mappingOpt, baseIRI, outputOpt, verboseOpt).mapN(SchemaMapping)
    }

  lazy val validateCommand: Opts[Validate] = 
    Opts.subcommand("validate", "Validate RDF data using a schema and a shape map") {
      (schemaSpec, dataSpec, shapeMapSpec, baseIRI, showResultFormatOpt, outputOpt, verboseOpt)
      .mapN(Validate)
    }

  lazy val shapePathValidateCommand: Opts[ShapePathEval] =
    Opts.subcommand("shapePath","Validate a shape path") {
      (schemaSpec, shapePathOpt, baseIRI,outputOpt, verboseOpt)
      .mapN(ShapePathEval)
    }

  lazy val prefixMapPath: Opts[Option[Path]] = Opts.option[Path]("prefixMapPath","path containing prefix map declarations (Wikidata by default)").orNone

  lazy val wikibaseCommand: Opts[WikibaseValidate] = 
    Opts.subcommand("wikibase", "Validate RDF data from wikibase") {
      (schemaSpec, endpoint, prefixMapPath, shapeMapSpec, baseIRI, showResultFormatOpt, outputOpt, verboseOpt)
      .mapN(WikibaseValidate)
    }

  lazy val manifestOpt = Opts.option[Path]("manifest", short = "m", help = "Path to manifest file.")

  lazy val manifestCommand: Opts[Manifest] =
    Opts.subcommand("manifest", "Run manifest file containing tests") {
      (manifestOpt, verboseOpt).mapN(Manifest)
    }
  

  def info(msg: String, verbose: Boolean): IO[Unit] = 
   if (verbose) IO.println(msg)
   else IO(())

  override def main: Opts[IO[ExitCode]] =
   (schemaMappingCommand orElse 
    validateCommand orElse
    shapePathValidateCommand orElse 
    manifestCommand orElse
    wikibaseCommand
   ).map {
     case smc: SchemaMapping => doSchemaMapping(smc) 
     case vc : Validate => doValidate(vc)
     case spc: ShapePathEval => doShapePathEval(spc)
     case mf: Manifest => runManifest(mf)
     case wc: WikibaseValidate => doWikibaseValidate(wc)
   }.map(
     _.handleErrorWith(infoError)
   ) 

   private def infoError(err: Throwable): IO[ExitCode] =
    IO.println(s"Error ${err.getLocalizedMessage()}") *> IO(ExitCode.Error)

  

   private def doSchemaMapping(smc: SchemaMapping): IO[ExitCode] = for {
       schema <- getSchema(smc.schemaSpec, smc.baseIRI)
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

   private def getSchema(schemaSpec: SchemaSpec, baseIRI: Option[IRI]): IO[Schema] = schemaSpec match {
     case SchemaPath(schema, schemaFormat) => 
       Schema.fromFile(schema.toFile().getAbsolutePath(), schemaFormat, baseIRI, None)
     case SchemaURI(uri) => Schema.fromIRI(IRI(uri), baseIRI)
   }

   private def getRDFData(dataSpec: DataSpec, baseIRI: Option[IRI]): IO[Resource[IO,RDFReader]] = dataSpec match {
     case DataPath(dataPath, dataFormat) => RDFAsJenaModel.fromURI(dataPath.toUri().toString(), dataFormat.getOrElse(defaultDataFormat), baseIRI)
     case EndpointOpt(uri) => IO(Resource.pure[IO,RDFReader](Endpoint(IRI(uri))))
   }
   

   private def doValidate(vc: Validate): IO[ExitCode] = 
    for {
        res1 <- getRDFData(vc.dataSpec, vc.baseIRI) // RDFAsJenaModel.fromURI(vc.data.toUri().toString(),vc.dataFormat,None)
        res2 <- RDFAsJenaModel.empty
        vv <- (res1,res2).tupled.use { 
      case (rdf,builder) => for {
       nodesPrefixMap <- rdf.getPrefixMap
       schema <- getSchema(vc.schemaSpec, vc.baseIRI) 
       resolvedSchema <- ResolvedSchema.resolve(schema,None)
       shapeMap <- getShapeMapFromFile(vc.shapeMapSpec.shapeMap,vc.shapeMapSpec.shapeMapFormat,nodesPrefixMap, schema.prefixMap, vc.baseIRI)
       fixedMap <- ShapeMap.fixShapeMap(shapeMap, rdf, nodesPrefixMap, resolvedSchema.prefixMap)
       result   <- Validator.validate(resolvedSchema, fixedMap, rdf, builder, vc.verbose)
       resultShapeMap <- result.toResultShapeMap
       _             <- showResult(resultShapeMap, vc.showResultFormat) 
      } yield ExitCode.Success}
    } yield vv

   private def doWikibaseValidate(wc: WikibaseValidate): IO[ExitCode] = 
    getPrefixMap(wc.prefixMapPath).flatMap(pm => 
    for {
        res1 <- getWikibaseRDF(wc.endpoint, pm) 
        res2 <- RDFAsJenaModel.empty
        vv <- (res1,res2).tupled.use { 
      case (rdf,builder) => for {
       nodesPrefixMap <- rdf.getPrefixMap
       schema <- getSchema(wc.schemaSpec, wc.baseIRI) 
       resolvedSchema <- ResolvedSchema.resolve(schema,None)
       shapeMap <- getShapeMapFromFile(wc.shapeMapSpec.shapeMap,wc.shapeMapSpec.shapeMapFormat,nodesPrefixMap, schema.prefixMap, wc.baseIRI)
       fixedMap <- ShapeMap.fixShapeMap(shapeMap, rdf, nodesPrefixMap, resolvedSchema.prefixMap)
       result   <- Validator.validate(resolvedSchema, fixedMap, rdf, builder, wc.verbose)
       resultShapeMap <- result.toResultShapeMap
       _             <- showResult(resultShapeMap, wc.showResultFormat) 
      } yield ExitCode.Success}
    } yield vv)

  private def getPrefixMap(maybePath: Option[Path]): IO[PrefixMap] = maybePath match {
    case None => IO(WikibaseRDF.wikidataPrefixMap)
    case Some(path) => {
      RDFAsJenaModel.fromURI(path.toUri().toString(), "Turtle")
      .flatMap(_.use(rdf => rdf.getPrefixMap))
    }
  }


  private def getWikibaseRDF(ep: EndpointOpt, pm: PrefixMap): IO[Resource[IO,WikibaseRDF]] = 
   WikibaseRDF.fromEndpoint(IRI(ep.uri), pm)

   private def doShapePathEval(spc: ShapePathEval): IO[ExitCode] = for {
     schema <- getSchema(spc.schemaSpec, spc.baseIRI) 
     shapePath <- IO.fromEither(ShapePath.fromString(spc.shapePath, "Compact", None, schema.prefixMap).leftMap(err => new RuntimeException(s"Error parsing shapePath: ${err}")))
     result <- { 
       val (ls,v) = ShapePath.eval(shapePath,schema)
       IO.println(ls.map(_.toString).mkString("\n")) *>
       v.pure[IO]
     } 
   } yield ExitCode.Success 

  private def showResult(result: ResultShapeMap, showResultFormat: String): IO[Unit] =
    IO.println(result.serialize(showResultFormat).fold(
      err => s"Error serializing ${result} with format ${showResultFormat}: $err", 
      identity)
    )


  private def getShapeMapFromFile(filePath: Path, 
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

  private def runManifest(mf: Manifest): IO[ExitCode] =
    for {
      eitherManifest <- RDF2Manifest.read(mf.manifestPath, "Turtle", None, true).attempt
      exitCode <- eitherManifest.fold(
        e =>
          IO.println(s"Error reading manifest: $e") *>
          IO(ExitCode.Error),
        manifest =>
          IO.println(
            s"""|Manifest read with ${manifest.entries.length} entries
          |Number of includes: ${manifest.includes.length}""".stripMargin
          ) *>
          IO(ExitCode.Success)
      )
    } yield exitCode

  // TODO: Move to utils  


/*  def writeContents(path: Path, contents: String): IO[Unit] = {
    println(s"Contents:\n${contents}\n-------------")
    Stream.resource(Blocker[IO]).flatMap(blocker =>
     Stream.emits(contents)
     .covary[IO]
     .chunkN(4096)
     .map(_.toVector.mkString)
     .through(text.utf8Encode)
     .through(io.file.writeAll(path, blocker))
    )
    .compile.drain
  }

  def getContents(fileName: String): IO[CharSequence] = {
    Stream.resource(Blocker[IO])
    .flatMap(blocker =>
      fs2.io.file.readAll[IO](Paths.get(fileName), blocker,4096)
      .through(text.utf8Decode)
    )
    .compile
    .string
  }
*/

}
