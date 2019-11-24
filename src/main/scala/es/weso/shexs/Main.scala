package es.weso.shexs
import cats.data.EitherT
import cats.effect._
import cats.syntax.all._
import scala.concurrent.duration._
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
       eitherValue <- runManifest(opts.manifest()).value
       _ <- eitherValue.fold(s => IO(println(s"Error: $s")), v => IO(println(s"End: $v")))
      } yield ()
    } else {
      IO.sleep(1.second) *> IO(println("Hello world!"))
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

 private def runManifest(manifest: String): EitherT[IO,String,Unit] = for {
  manifest <- RDF2Manifest.read(manifest,"Turtle",None, true)
  _ <- EitherT.liftF(IO(println(s"Manifest read!")))
 } yield ()
 

}
