package es.weso.shexs

import com.monovore.decline.Opts
import cats.data.Validated._
import cats.implicits._
import es.weso.utils.VerboseLevel
import es.weso.utils.VerboseLevel._

object VerboseLevelOpt {
  def verboseLevel: Opts[VerboseLevel] =
    Opts
      .option[String]("verbose", short = "v", help = s"verbose level ($showVerboseLevels)")
      .mapValidated(n =>
        VerboseLevel.fromString(n) match {
          case Some(v) => valid(v)
          case None    => invalidNel(s"Unknown value for verbose level: $n")
        }
      )
      .withDefault(Nothing)
}
