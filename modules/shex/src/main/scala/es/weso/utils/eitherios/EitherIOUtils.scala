package es.weso.utils.eitherios
import cats._
import cats.data._
import implicits._
import cats.effect.IO

object EitherIOUtils {

  def partitionEitherIOS[A, B](vs: List[EitherT[IO, A, B]]): IO[(List[A], List[B])] = {
    val zero: IO[(List[A], List[B])] = IO((List(), List()))
    def cmb(next: IO[(List[A], List[B])], c: EitherT[IO, A, B]): IO[(List[A], List[B])] = for {
      pairs <- next
      e     <- c.value
    } yield {
      val (as, bs) = pairs
      e.fold(a => (a :: as, bs), b => (as, b :: bs))
    }
    vs.foldLeft(zero)(cmb)
  }

  def injectPairLs[A, B, C](v: List[(A, EitherT[IO, B, C])]): List[EitherT[IO, (A, B), (A, C)]] = {
    v.map(pair => {
      val (a, e) = pair
      e.bimap(x => (a, x), y => (a, y))
    })
  }

  def eitherStr2IO[A](e: Either[String, A]): IO[A] =
    MonadError[IO, Throwable].rethrow(IO(e.leftMap(new Exception(_))))

  def info_io(msg: String): IO[Unit] = IO {
    println(msg)
  }

  def info_es(msg: String): Either[String, Unit] = {
    println(msg)
    Right(())
  }

  def info_esio[F[_]: Applicative](msg: String): EitherT[F, String, Unit] = {
    println(msg)
    EitherT.fromEither[F](Right(()))
  }

  def eitherT2io[A, B](e: EitherT[IO, A, B]): IO[Either[A, B]] = {
    println(s"EitherT2io")
    e.value.flatMap(
      _.fold(
        err => {
          println(s"Error: $err")
          IO(Left(err))
        },
        (x: B) => IO(x.asRight)
      )
    )
  }

}
