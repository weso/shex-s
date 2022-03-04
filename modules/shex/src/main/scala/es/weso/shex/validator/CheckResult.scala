package es.weso.shex.validator
import cats._
import cats.implicits._

case class CheckResult[E: Show, A: Show, Log: Show](log: Log, eitherResult: Either[E, A]) {

  def getResult: Option[A] = toEither.toOption

  def isOK: Boolean = eitherResult.isRight

  def errors: Seq[E] =
    eitherResult.fold(e => List(e), _ => Seq())

  def results: List[A] = {
    eitherResult.fold(_ => List(), x => List(x))
  }

  def show: String = {
    val result = if (isOK) {
      val first = results.head
      "OK. Result: " ++ "\n" ++
        Show[A].show(first)
    } else "Not OK. Error: " ++ "\n" ++ errors.map(e => Show[E].show(e)).mkString("\n")
    val sb = new StringBuilder
    sb ++= result
    sb ++= "\n----------------------------log-----------------------\n"
    sb ++= log.show
    sb.toString
  }

  def toEither: Either[E, A] = eitherResult

}
