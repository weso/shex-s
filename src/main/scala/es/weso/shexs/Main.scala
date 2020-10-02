package es.weso.shexs
import cats.effect._
import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import es.weso.shextest.manifest._

object Main extends IOApp {
  // implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  def run(args: List[String]): IO[ExitCode] = {
    val opts = new MainOpts(args.toArray, errorDriver)
    for {
      _ <- IO(opts.verify())
      code <- run(opts)
    } yield code
  }

  private def run(opts: MainOpts): IO[ExitCode] = {
    if (opts.manifest.isDefined) { 
      for {
       eitherValue <- runManifest(opts.manifest()).attempt
       exitCode <- eitherValue.fold(
         s => IO {
           println(s"Error: $s")
           ExitCode.Error
         },
         v => IO {
           println(s"End: $v")
           ExitCode.Success
         }
       )
      } yield exitCode
    } else {
      IO {
        println(s"ShEx-s!")
        opts.printHelp()
        ExitCode.Success
      }
    }
  }

  private def errorDriver(e: Throwable, scallop: Scallop) = e match {
    case Help(s) => {
      println(s"Help: $s")
      scallop.printHelp
      sys.exit(0)
    }
    case _ => {
      println(s"Error: ${e.getMessage}")
      scallop.printHelp
      sys.exit(1)
    }
  }

 private def runManifest(manifest: String): IO[Unit] = for {
  manifest <- RDF2Manifest.read(manifest,"Turtle",None, true)
  _ <- IO(println(s"Manifest read with ${manifest.entries.length} entries. Number of includes: ${manifest.includes.length}"))
 } yield ()

}
