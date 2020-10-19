package es.weso.shexs
// import java.io.File
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
import es.weso.shapeMaps.FixedShapeMap
import es.weso.shapeMaps.ResultShapeMap

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
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
      _ <- ifOpt(opts.dataFormat, setDataFormat)
      res <- getData(opts)
      _ <- res.use {
        case (rdf,builder) =>
          for {
            _              <- ifOpt(opts.showDataFormat, setShowDataFormat)
            _              <- ifOpt(opts.showSchemaFormat, setShowSchemaFormat)
            _              <- ifOpt(opts.showResultFormat, setShowResultFormat)
            _              <- ifOpt(opts.schemaFormat, setSchemaFormat)
            _              <- ifOpt(opts.schemaFile, setSchemaFile)
            _              <- ifOptB(opts.showData, showData(rdf))
            _              <- ifOpt(opts.shapeMap, setShapeMap(rdf))
            _              <- ifOptB(opts.showSchema, showSchema)
            resolvedSchema <- getResolvedSchema()
            fixedMap       <- getFixedMap(rdf, resolvedSchema)
            result         <- fromIO(Validator.validate(resolvedSchema, fixedMap, rdf, builder))
            resultShapeMap <- fromIO(result.toResultShapeMap)
            _              <- showResult(resultShapeMap) // putStrLn(s"Result\n${resultShapeMap.toString}"))
          } yield ()
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

  private def showResult(result: ResultShapeMap): IOS[Unit] =
    for {
      state <- getState
      str = state.showResultFormat.toUpperCase match {
        case "COMPACT" => result.toString
        case "JSON"    => result.toJson.spaces2
      }
      _ <- fromIO(putStrLn(str))
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

  /* private def setData(dataStr: String): IOS[Unit] = for {
    state <- getState
    // _ <- modifyData(getRDFData(dataStr, state.dataFormat))
  } yield () */

  private def setShapeMap(rdf: RDFReader)(shapeMap: String): IOS[Unit] =
    for {
      state <- getState
      pm    <- fromIO(rdf.getPrefixMap)
      // _ <- fromIO(putStrLn(s"PrefixMap: ${pm.toString}"))
      sm <- fromEither(ShapeMap.fromString(shapeMap, state.shapeMapFormat, None, pm, state.schema.prefixMap))
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

  private def getRDFDataFromFile(fileName: String, dataFormat: String): IOS[Resource[IOS, RDFReader]] = ok {
    RDFAsJenaModel.fromFile(Paths.get(fileName).toFile, dataFormat).mapK(cnv)
  }

  private def getSchemaFromFile(fileName: String, schemaFormat: String): IOS[Schema] =
    fromIO(Schema.fromFile(fileName, schemaFormat))

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

}
