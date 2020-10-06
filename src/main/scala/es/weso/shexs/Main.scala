package es.weso.shexs
import java.io.File
import java.nio.file.Paths

import cats.arrow.FunctionK
import cats.data.StateT
import cats.effect._
import cats.effect.Console.io._
import cats.implicits._
import cats.~>
import es.weso.rdf.RDFReader
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shapeMaps.ShapeMap
import es.weso.shex.{ResolvedSchema, Schema}
import es.weso.shex.validator.Validator
import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import es.weso.shextest.manifest._
import es.weso.shextest.manifest.ShExManifest

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val opts = new MainOpts(args.toArray, errorDriver)
    for {
      _ <- IO(opts.verify())
      code <- run(opts)
    } yield code
  }

  type IOS[A] = StateT[IO,MainState,A]

  private def run(opts: MainOpts): IO[ExitCode] = {
    val r = for {
      initialState <- MainState.initial
      pair <- runS(opts).run(initialState)
      (state, code) = pair
      _ <- state.data.use(rdf =>
        for {
          _ <- if (opts.showData()) showData(rdf, state.showDataFormat)
               else IO.pure(())
          resolvedSchema <- ResolvedSchema.resolve(state.schema, None)
          prefixMap <- rdf.getPrefixMap
          fixedMap <- ShapeMap.fixShapeMap(state.shapeMap,rdf,prefixMap, resolvedSchema.prefixMap)
          result <- Validator.validate(resolvedSchema,fixedMap,rdf)
          resultShapeMap <- result.toResultShapeMap
          _ <- putStrLn(s"Result\n${resultShapeMap.toString}")
      } yield ())
    } yield ()
    r.attempt.flatMap(_.fold(
      e => {
        putStrLn(s"Error: ${e.getMessage}") *>
          IO(ExitCode.Error)
      },
      _ => IO(ExitCode.Success)
    ))
  }

  private def showData(rdf: RDFReader, dataFormat: String): IO[Unit] =
    rdf.serialize(dataFormat).flatMap(putStrLn)


  private def ok[A](v: A): IOS[A] = v.pure[IOS]

  private def runS(opts: MainOpts): IOS[ExitCode] = for {
    _ <- ifOpt(opts.manifest, mf => StateT.liftF(runManifest(mf)))
    _ <- ifOpt(opts.dataFormat,setDataFormat)
    _ <- ifOpt(opts.data, setData)
    _ <- ifOpt(opts.dataFile,setDataFile)
    _ <- ifOpt(opts.schemaFile,setSchemaFile)
    _ <- ifOpt(opts.schemaFormat,setSchemaFormat)
    _ <- ifOpt(opts.shapeMap,setShapeMap)
    _ <- ifOpt(opts.showDataFormat,setShowDataFormat)
  } yield ExitCode.Success

/*  private def infoState(): IOS[Unit] = for {
   state <- getState
   _ <- fromIO(
     for {
       n <- state.data.getNumberOfStatements()
       _ <- putStrLn(s"RDF with $n statements")
     } yield())
  } yield () */

/*  private def showRDFData(): IOS[Unit] = for {
    state <- getState
    _ <- fromIO(for {
      str <- state.data.serialize(state.showDataFormat)
      _ <- putStrLn(str)
    } yield ())
  } yield () */

  private def setDataFormat(df: String): IOS[Unit] =
    StateT.modify(s => s.copy(dataFormat = df))

  private def setSchemaFormat(sf: String): IOS[Unit] =
    StateT.modify(s => s.copy(schemaFormat = sf))

  private def setShowDataFormat(sf: String): IOS[Unit] =
    StateT.modify(s => s.copy(showDataFormat = sf))

  private def setData(dataStr: String): IOS[Unit] = for {
    state <- getState
    _ <- modifyData(getRDFData(dataStr, state.dataFormat))
  } yield ()

  private def setShapeMap(shapeMap: String): IOS[Unit] = for {
    state <- getState
    sm <- fromEither(ShapeMap.fromString(shapeMap,state.shapeMapFormat))
    _ <- modifyS(s => s.copy(shapeMap = sm))
  } yield ()

  private def fromEither[A](either: Either[String,A]): IOS[A] =
    either.fold(
      e => fromIO(IO.raiseError(new RuntimeException(s"Error parsing shapeMap: $e"))),
      ok(_)
    )

  private def fromIO[A](io: IO[A]): IOS[A] =
    StateT.liftF(io)

  private def getState: IOS[MainState] = StateT.get[IO, MainState]

  private def setDataFile(fileName: String): IOS[Unit] = for {
    state <- getState
    _ <- modifyData(getRDFDataFromFile(fileName, state.dataFormat))
  } yield ()

  private def setSchemaFile(fileName: String): IOS[Unit] = for {
    state <- getState
    sch <- getSchemaFromFile(fileName, state.schemaFormat)
    _ <- modifyS(s => s.copy(schema = sch))
  } yield ()

  private def modifyS(fn: MainState => MainState): IOS[Unit] =
    StateT.modify(fn)

  private def modifyData(rdf: Resource[IO,RDFReader]): IOS[Unit] =
    StateT.modify(s => s.copy(data = rdf))

/*  private def cnv: IO ~> IOS = new FunctionK[IO, IOS] {
    def apply[A](io: IO[A]): IOS[A] = fromIO(io)
  } */

  private def getRDFData(data: String, dataFormat: String): Resource[IO,RDFReader] = {
    RDFAsJenaModel.fromString(data, dataFormat)
//      .mapK(cnv)
  }

  private def getRDFDataFromFile(fileName: String, dataFormat: String): Resource[IO,RDFReader] = {
    RDFAsJenaModel.fromFile(Paths.get(fileName).toFile, dataFormat)
  //    .mapK(cnv)
  }

  private def getSchemaFromFile(fileName: String, schemaFormat: String): IOS[Schema] =
   fromIO(Schema.fromFile(fileName,schemaFormat))


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

  private def ifOptIO(opt: ScallopOption[String], action: String => IO[Unit]): IO[Unit] = {
    if (opt.isDefined) action(opt())
    else ().pure[IO]
  }


  private def ifOpt(opt: ScallopOption[String], action: String => IOS[Unit]): IOS[Unit] = {
   if (opt.isDefined) action(opt())
   else ().pure[IOS]
 }

 private def runManifest(manifest: String): IO[Unit] = for {
  eitherManifest <- RDF2Manifest.read(manifest,"Turtle",None, true).attempt
  _ <- eitherManifest.fold(
    e =>  putStrLn(s"Error reading manifest: $e") *>
          ShExManifest.empty.pure[IO],
    manifest => putStrLn(
      s"""|Manifest read with ${manifest.entries.length} entries
          |Number of includes: ${manifest.includes.length}""".stripMargin
    )
  )
 } yield ()

}
