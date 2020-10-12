package es.weso.shex.validator

import cats.implicits._
import es.weso.shapeMaps.ResultShapeMap
import cats.effect.IO
import cats.implicits._

case class Result(e: Either[ShExError, ResultShapeMap]) {

  def toEitherS: Either[String, ResultShapeMap] = e.leftMap(_.toString)
  def toEither: Either[ShExError,ResultShapeMap] = e
  def toResultShapeMap: IO[ResultShapeMap] = cnv(e)

  private def cnv[A](e: Either[ShExError,A]): IO[A] = 
    IO.fromEither(e)
/*    MonadError[IO,Throwable].rethrow(
      IO(e.leftMap((err: ShExError) => new Exception(err.toString)))) */
  
}