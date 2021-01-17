package es.weso.shexs
import java.nio.file.Paths
import cats.arrow.FunctionK
import cats.data.StateT
import cats.effect._
import cats.effect.Console.io._
import cats.implicits._
import cats.~>
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shapeMaps.ShapeMap
import es.weso.shex.{ResolvedSchema, Schema}
import es.weso.shex.validator.Validator
import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import es.weso.shextest.manifest._
import es.weso.shextest.manifest.ShExManifest
import es.weso.shapeMaps._ 
import fs2._
import es.weso.shex.validator.ValidationLog
import com.monovore.decline._
import com.monovore.decline.effect._
import buildinfo._
import java.nio.file.Path
import es.weso.shapepath.schemamappings.SchemaMappings
import es.weso.shex.implicits.showShEx._

object Main extends CommandIOApp(
  name="shex-s", 
  header = "ShEx-Scala command line tool",
  version = BuildInfo.version
  ) {

  case class SchemaMappingCommand(schema: Path, schemaFormat: String, mapping: Path, output: Option[Path], verbose: Boolean)
  case class ValidateCommand(schema: Path, schemaFormat: String, data: Path, dataFormat: String, shapeMap: Path, shapeMapFormat: String, output: Option[Path], verbose: Boolean)

  val availableSchemaFormats = List("ShExC", "ShExJ")
  val defaultSchemaFormat = availableSchemaFormats.head
  val availableSchemaFormatsStr = availableSchemaFormats.mkString(",")

  val availableDataFormats = List("Turtle", "NTriples","RDF/XML","JSON-LD")
  val defaultDataFormat = availableDataFormats.head
  val availableDataFormatsStr = availableDataFormats.mkString(",")

  val availableShapeMapFormats = List("Compact", "JSON")
  val defaultShapeMapFormat = availableShapeMapFormats.head
  val availableShapeMapFormatsStr = availableShapeMapFormats.mkString(",")

  val schemaOpt = Opts.option[Path]("schema", short = "s", help = "Path to ShEx file.")
  val schemaFormatOpt = Opts.option[String]("schemaFormat", metavar = "format", help = s"Schema format, default = ($defaultSchemaFormat). Possible values = ($availableSchemaFormatsStr)").withDefault(defaultSchemaFormat)
  val outputOpt = Opts.option[Path]("output","Output to file (default = console)").orNone
  val verboseOpt = Opts.flag("verbose", "show extra information").orFalse
  val mappingOpt = Opts.option[Path]("mapping", short = "m", metavar = "mappings-file", help = "Path to Mappings file.")
  val dataOpt = Opts.option[Path]("data", short = "d", help = "Path to data file.")
  val dataFormatOpt = Opts.option[String]("dataFormat", help = s"Data format. Default=$defaultDataFormat, available=$availableDataFormatsStr").withDefault(defaultDataFormat)
  val shapeMapOpt = Opts.option[Path]("shapeMap", short = "sm", help = "Path to shapeMap file.")
  val shapeMapFormatOpt = Opts.option[String]("shapeMapFormat", help = s"ShapeMap format, default=$defaultShapeMapFormat, available formats=$availableShapeMapFormats").withDefault(defaultShapeMapFormat)

  val schemaMappingCommand: Opts[SchemaMappingCommand] = 
    Opts.subcommand("mapping", "Convert a schema through a mapping") {
      (schemaOpt,schemaFormatOpt, mappingOpt, outputOpt, verboseOpt).mapN(SchemaMappingCommand)
    }

  val validateCommand: Opts[ValidateCommand] = 
    Opts.subcommand("validate", "Validate RDF data using a schema and a shape map") {
      (schemaOpt,schemaFormatOpt, dataOpt, dataFormatOpt, shapeMapOpt, shapeMapFormatOpt, outputOpt, verboseOpt)
      .mapN(ValidateCommand)
    }


  def info(msg: String, verbose: Boolean): IO[Unit] = 
   if (verbose) putStrLn(msg)
   else IO(())

  override def main: Opts[IO[ExitCode]] =
   (schemaMappingCommand orElse validateCommand).map {
     case smc: SchemaMappingCommand => doSchemaMapping(smc) 
     case vc : ValidateCommand => doValidate(vc) 
   }

   private def doSchemaMapping(smc: SchemaMappingCommand): IO[ExitCode] = for {
       schema <- Schema.fromFile(smc.schema.toFile().getAbsolutePath(), smc.schemaFormat, None, None)
       mappingStr <- getContents(smc.mapping.toFile().getAbsolutePath())
       mapping <- IO.fromEither(SchemaMappings
        .fromString(mappingStr.toString)
        .leftMap(err => new RuntimeException(s"Error parsing schema mappings: ${err}"))
        )
       newSchema <- IO.fromEither(mapping.convert(schema)
        .leftMap(err => new RuntimeException(s"Error converting schema: ${err}")))
       _ <- smc.output match {
         case None => putStrLn(newSchema.show)
         case Some(outputPath) => for { 
           _ <- writeContents(outputPath, newSchema.show)
           _ <- putStrLn(s"Output saved in ${outputPath}")
         } yield ()  
       } 
     } yield ExitCode.Success

   private def doValidate(vc: ValidateCommand): IO[ExitCode] = ???


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


  private def getShapeMapFromFile(fileName: String, 
                                  shapeMapFormat: String,
                                  nodesPrefixMap: PrefixMap,
                                  shapesPrefixMap: PrefixMap): IOS[ShapeMap] =
    for {
      resolvedName <- resolve(fileName)
      str <- fromIO(getContents(resolvedName).handleErrorWith(e => IO.raiseError(new RuntimeException(s"Error obtaining shapeMap from file: ${resolvedName} with format ${shapeMapFormat}: ${e.getMessage()}"))))
      sm <- fromEither(ShapeMap.fromString(str.toString, shapeMapFormat, None, nodesPrefixMap,shapesPrefixMap))
    } yield sm 


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
  private def runManifest(manifest: String): IO[Unit] =
    for {
      eitherManifest <- RDF2Manifest.read(manifest, "Turtle", None, true).attempt
      _ <- eitherManifest.fold(
        e =>
          putStrLn(s"Error reading manifest: $e") *>
            ShExManifest.empty.pure[IO],
        manifest =>
          putStrLn(
            s"""|Manifest read with ${manifest.entries.length} entries
          |Number of includes: ${manifest.includes.length}""".stripMargin
          )
      )
    } yield ()

  // TODO: Move to utils  

  def writeContents(path: Path, contents: String): IO[Unit] = {
    Stream.resource(Blocker[IO]).flatMap(blocker =>
     Stream.emits(contents.split("\n"))
     .covary[IO]
     .through(text.utf8Encode)
     .through(io.file.writeAll(path, blocker))
    ).compile.drain
  }

  def getContents(fileName: String): IO[CharSequence] = {
    val path = Paths.get(fileName)
    val decoder: Pipe[IO,Byte,String] = fs2.text.utf8Decode
    Stream.resource(Blocker[IO]).flatMap(blocker =>
      fs2.io.file.readAll[IO](path, blocker,4096).through(decoder)
    ).compile.string
  }


}
