package es.weso.shexs
import java.nio.file.Paths
// import cats.arrow.FunctionK
// import cats.data.StateT
import cats.effect._
import cats.effect.Console.io._
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
import fs2._
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

object Main extends CommandIOApp(
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
   if (verbose) putStrLn(msg)
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
    putStrLn(s"Error ${err.getLocalizedMessage()}") *> IO(ExitCode.Error)

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
           _ <- putStrLn(warnings.map(_.toString).mkString("\n"))
         } yield s
       )
       _ <- smc.output match {
         case None => putStrLn(newSchema.show)
         case Some(outputPath) => for { 
           _ <- writeFile(outputPath.toFile().getAbsolutePath(), newSchema.show)
           _ <- putStrLn(s"Output saved in ${outputPath}")
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
       putStrLn(ls.map(_.toString).mkString("\n")) *>
       v.pure[IO]
     } 
   } yield ExitCode.Success 

  private def showResult(result: ResultShapeMap, showResultFormat: String): IO[Unit] =
    putStrLn(result.serialize(showResultFormat).fold(
      err => s"Error serializing ${result} with format ${showResultFormat}: $err", 
      identity)
    )


/*  def run(args: List[String]): IO[ExitCode] = {
    val opts = new MainOpts(args.toArray, errorDriver)
    for {
      _    <- IO(opts.verify())
      code <- run(opts)
    } yield code
  }

  private type IOS[A] = StateT[IO, MainState, A]

  private def ok[A](v: A): IOS[A] = v.pure[IOS]

  private def run(opts: MainOpts): IO[ExitCode] = {
    runS(opts)
      .run(MainState.initial)
      .attempt
      .flatMap(
        _.fold(
          e => {
            putStrLn(s"Error: ${e.getMessage}") *>
              IO(ExitCode.Error)
          },
          _ => IO(ExitCode.Success)
        )
      )
  }

  private def runS(opts: MainOpts): IOS[ExitCode] =
    for {
      _ <- ifOpt(opts.manifest, mf => StateT.liftF(runManifest(mf)))
      _ <- ifOpt(opts.folder, setFolder)
      _ <- ifOpt(opts.dataFormat, setDataFormat)
      res <- getData(opts)
      _ <- res.use {
        case (rdf,builder) =>
          (for {
            _              <- ifOpt(opts.showDataFormat, setShowDataFormat)
            _              <- ifOpt(opts.showSchemaFormat, setShowSchemaFormat)
            _              <- ifOpt(opts.showResultFormat, setShowResultFormat)
            _              <- ifOpt(opts.schemaFormat, setSchemaFormat)
            _              <- ifOpt(opts.shapeMapFormat, setShapeMapFormat)
            _              <- ifOpt(opts.schemaFile, setSchemaFile)
            _              <- ifOptB(opts.showData, showData(rdf))
            _              <- ifOpt(opts.shapeMap, setShapeMap(rdf))
            _              <- ifOpt(opts.shapeMapFile, setShapeMapFromFile(rdf))
            _              <- ifOptB(opts.showSchema, showSchema)
            resolvedSchema <- getResolvedSchema()
            fixedMap       <- getFixedMap(rdf, resolvedSchema)
            result         <- fromIO(Validator.validate(resolvedSchema, fixedMap, rdf, builder, opts.verbose()))
            _              <- showLog(result.toValidationLog)
            resultShapeMap <- fromIO(result.toResultShapeMap)
            _              <- showResult(resultShapeMap) // putStrLn(s"Result\n${resultShapeMap.toString}"))
          } yield ()).handleErrorWith(t => ok { println(s"Error: ${t.getMessage}")})
        }
    } yield ExitCode.Success

  private def showSchema: IOS[Unit] =
    for {
      state <- getState
      str   <- fromIO(RDFAsJenaModel.empty.flatMap(_.use(empty => Schema.serialize(state.schema,state.schemaFormat, None, empty))))
      _     <- fromIO(putStrLn(str))
    } yield ()

  private def showData(rdf: RDFReader): IOS[Unit] =
    for {
      state <- getState
      str   <- fromIO(rdf.serialize(state.dataFormat))
      _     <- fromIO(putStrLn(str))
    } yield ()

  private def showLog(log: ValidationLog): IOS[Unit] =
    for {
      _ <- fromIO(putStrLn(log.show))
    } yield ()


  private def showResult(result: ResultShapeMap): IOS[Unit] =
    for {
      state <- getState
      _ <- fromIO(
        putStrLn(result.serialize(state.showResultFormat).fold(err => s"Error serializing ${result} with format ${state.showResultFormat}: $err", identity))
      )
    } yield ()

  private def getResolvedSchema(): IOS[ResolvedSchema] =
    for {
      state          <- getState
      resolvedSchema <- fromIO(ResolvedSchema.resolve(state.schema, None))
    } yield resolvedSchema

  private def getFixedMap(rdf: RDFReader, resolvedSchema: ResolvedSchema): IOS[FixedShapeMap] =
    for {
      state     <- getState
      prefixMap <- fromIO(rdf.getPrefixMap)
      fixedMap  <- fromIO(ShapeMap.fixShapeMap(state.shapeMap, rdf, prefixMap, resolvedSchema.prefixMap))
    } yield fixedMap

  private def cnvResource[A](r: Resource[IO,A]): Resource[IOS,A] = 
    r.mapK(cnv)

  private def pairResource[A,B](r1: Resource[IOS,A], r2: Resource[IOS,B]): Resource[IOS,(A,B)] = for {
    v1 <- r1
    v2 <- r2
  } yield (v1,v2)

  private def getData(opts: MainOpts): IOS[Resource[IOS, (RDFReader,RDFBuilder)]] = 
  for {
    state <- getState
    rdf <- if (opts.data.isDefined) {
      getRDFData(opts.data(), state.dataFormat)
    } else if (opts.dataFile.isDefined) {
      getRDFDataFromFile(opts.dataFile(), state.dataFormat)
    } else for {
      re <- fromIO(RDFAsJenaModel.empty)
      e = cnvResource(re)  
    } yield e
    emptyRes <- fromIO(RDFAsJenaModel.empty)
    builder = cnvResource(emptyRes)
  } yield pairResource(rdf,builder)  

  /*  private def infoState(): IOS[Unit] = for {
   state <- getState
   _ <- fromIO(
     for {
       n <- state.data.getNumberOfStatements()
       _ <- putStrLn(s"RDF with $n statements")
     } yield())
  } yield () */

  /*private def showRDFData(rdf: RDFReader): IOS[Unit] = for {
    state <- getState
    _ <- fromIO(for {
      str <- rdf.serialize(state.showDataFormat)
      _ <- putStrLn(str)
    } yield ())
  } yield () */

  private def setFolder(folder: String): IOS[Unit] = 
    fromIO(IO(Paths.get(folder))).flatMap(path => 
    StateT.modify(s => s.copy(folder = path)))

  private def setDataFormat(df: String): IOS[Unit] =
    StateT.modify(s => s.copy(dataFormat = df))

  private def setSchemaFormat(sf: String): IOS[Unit] =
    StateT.modify(s => s.copy(schemaFormat = sf))

  private def setShowDataFormat(sf: String): IOS[Unit] =
    StateT.modify(s => s.copy(showDataFormat = sf))

  private def setShowSchemaFormat(sf: String): IOS[Unit] =
    StateT.modify(s => s.copy(showSchemaFormat = sf))

  private def setShowResultFormat(sf: String): IOS[Unit] =
    StateT.modify(s => s.copy(showResultFormat = sf))

  private def setShapeMapFormat(sf: String): IOS[Unit] =
    StateT.modify(s => s.copy(shapeMapFormat = sf))

  private def setShapeMap(rdf: RDFReader)(shapeMap: String): IOS[Unit] =
    for {
      state <- getState
      pm    <- fromIO(rdf.getPrefixMap)
      // _ <- fromIO(putStrLn(s"PrefixMap: ${pm.toString}"))
      sm <- fromEither(ShapeMap.fromString(shapeMap, state.shapeMapFormat, None, pm, state.schema.prefixMap))
      _  <- modifyS(s => s.copy(shapeMap = sm))
    } yield ()

  private def setShapeMapFromFile(rdf: RDFReader)(shapeMapFile: String): IOS[Unit] =
    for {
      state <- getState
      // _ <- fromIO(putStrLn(s"PrefixMap: ${pm.toString}"))
      pm <- fromIO(rdf.getPrefixMap)
      sm <- getShapeMapFromFile(shapeMapFile, state.shapeMapFormat, pm, state.schema.prefixMap)
      _  <- modifyS(s => s.copy(shapeMap = sm))
    } yield ()


  private def fromEither[A](either: Either[String, A]): IOS[A] =
    either.fold(
      e => fromIO(IO.raiseError(new RuntimeException(s"Error parsing shapeMap: $e"))),
      ok(_)
    )

  private def fromIO[A](io: IO[A]): IOS[A] =
    StateT.liftF(io)

  private def getState: IOS[MainState] = StateT.get[IO, MainState]

  /*  private def setDataFile(fileName: String): IOS[Unit] = for {
    state <- getState
    // _ <- modifyData(getRDFDataFromFile(fileName, state.dataFormat))
  } yield () */

  private def setSchemaFile(fileName: String): IOS[Unit] =
    for {
      state <- getState
      sch   <- getSchemaFromFile(fileName, state.schemaFormat)
      _     <- modifyS(s => s.copy(schema = sch))
    } yield ()

  private def modifyS(fn: MainState => MainState): IOS[Unit] =
    StateT.modify(fn)

  private def cnv: IO ~> IOS = new FunctionK[IO, IOS] {
    def apply[A](io: IO[A]): IOS[A] = fromIO(io)
  }

  private def getRDFData(data: String, dataFormat: String): IOS[Resource[IOS, RDFReader]] = 
  for {
   res <- fromIO(RDFAsJenaModel.fromString(data, dataFormat)) 
  } yield cnvResource(res)

  private def getRDFDataFromFile(fileName: String, dataFormat: String): IOS[Resource[IOS, RDFReader]] = for {
    resolvedName <- resolve(fileName)
    res <- fromIO(RDFAsJenaModel.fromURI(Paths.get(resolvedName).toUri().toString(), dataFormat))
  } yield res.mapK(cnv)
   
  private def getSchemaFromFile(fileName: String, schemaFormat: String): IOS[Schema] =
    resolve(fileName).flatMap(resolvedName =>  
    fromIO(Schema.fromFile(resolvedName, schemaFormat)))

  private def resolve(filename: String): IOS[String] = 
  for {
      state <- getState
      resolvedName <- fromIO(IO(state.folder.resolve(filename).toAbsolutePath().toString()))
  } yield resolvedName


 


  private def errorDriver(e: Throwable, scallop: Scallop) = e match {
    case Help(s) =>
      println(s"Help: $s")
      scallop.printHelp
      sys.exit(0)
    case _ =>
      println(s"Error: ${e.getMessage}")
      scallop.printHelp
      sys.exit(1)
  }

  private def ifOptB(opt: ScallopOption[Boolean], action: IOS[Unit]): IOS[Unit] =
    if (opt()) action
    else ().pure[IOS]

  private def ifOpt(opt: ScallopOption[String], action: String => IOS[Unit]): IOS[Unit] = {
    if (opt.isDefined) action(opt())
    else ().pure[IOS]
  }
*/

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
          putStrLn(s"Error reading manifest: $e") *>
          IO(ExitCode.Error),
        manifest =>
          putStrLn(
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
