package es.weso.shexs
import cats.effect._
import scala.concurrent.ExecutionContext
import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import es.weso.shextest.manifest._

object Main {
  // Needed for `IO.sleep`
  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  private def program(args: Array[String]): IO[Unit] = {
    val opts = new MainOpts(args, errorDriver)
    for {
      _ <- IO(opts.verify())
      _ <- run(opts)
    } yield ()
  }

  private def run(opts: MainOpts): IO[Unit] = {
    if (opts.manifest.isDefined) { 
      for {
       eitherValue <- runManifest(opts.manifest()).attempt
       _ <- eitherValue.fold(
         s => IO(println(s"Error: $s")),
         v => IO(println(s"End: $v"))
       )
      } yield ()
    } else {
      IO {
        println(s"ShEx-s!")
        opts.printHelp()
      }
    }
  }

  def main(args: Array[String]): Unit =
    program(args).unsafeRunSync

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
