import cats.effect._
import cats.syntax.all._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

object Main {
  // Needed for `IO.sleep`
  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)
  
  def program(args: List[String]): IO[Unit] =
    IO.sleep(1.second) *> IO(println("Hello world!"))
    
  def main(args: Array[String]): Unit =
    program(args.toList).unsafeRunSync
}