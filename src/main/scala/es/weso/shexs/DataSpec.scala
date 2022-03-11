package es.weso.shexs

import java.nio.file.Path
import java.net.URI
import com.monovore.decline.Opts
import cats.implicits._

abstract class DataSpec 
case class DataPath(dataPath: Path, dataFormat: Option[String]) extends DataSpec

object DataSpec {

 
 lazy val dataOpt = Opts.option[Path]("data", short = "d", help = "Path to data file.")

 lazy val dataPath: Opts[DataPath] = (dataOpt,DataFormat.dataFormatOpt).mapN {
    case (path,format) => DataPath(path,Some(format))
  }


 lazy val dataSpec: Opts[DataSpec] = dataPath orElse EndpointOpt.endpoint

}