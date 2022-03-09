package es.weso.shex.validator

import cats._
import cats.implicits._
import es.weso.shapemaps.ResultShapeMap
import cats.effect._

case class Result(e: Either[ShExError, (ValidationLog, ResultShapeMap)]) extends AnyVal {

  def toEitherS: Either[String, (ValidationLog, ResultShapeMap)]   = e.leftMap(_.toString)
  def toEither: Either[ShExError, (ValidationLog, ResultShapeMap)] = e
  def toResultShapeMap: IO[ResultShapeMap]                         = IO.fromEither(e.map(_._2))
  def toValidationLog: ValidationLog = e.fold(_ => Monoid[ValidationLog].empty, pair => pair._1)

}
