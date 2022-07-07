package es.weso.shexs

import com.monovore.decline.Opts
import java.nio.file.Path

object OutputOpt {
  lazy val outputOpt = Opts.option[Path]("output", "Output to file (default = console)").orNone
}
