package es.weso.shexs

import com.monovore.decline.Opts
import es.weso.shapemaps.ResultShapeMap
import cats.effect.IO

object ShowResult {

  lazy val showResultFormatOpt =
    Opts.option[String]("showResultFormat", help = s"showResultFormat").withDefault("details")

  def showResult(result: ResultShapeMap, showResultFormat: String): IO[Unit] =
    IO.println(
      result
        .serialize(showResultFormat)
        .fold(err => s"Error serializing ${result} with format ${showResultFormat}: $err", identity)
    )

}
