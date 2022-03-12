package es.weso.shexs

import com.monovore.decline.Opts
import java.net.URI

case class EndpointOpt(uri: URI) extends DataSpec

object EndpointOpt {
  lazy val endpoint: Opts[EndpointOpt] = UriOpt.uri("endpoint", "endpoint URL").map(EndpointOpt.apply)
}
