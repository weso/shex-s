package es.weso.shexs

import com.monovore.decline.Opts
import java.net.URI
import scala.util.Try
import cats.data.Validated

object UriOpt {
  def uri(name: String, help: String, short: String = ""): Opts[URI] =
    Opts
      .option[String](name, help = help, short = short)
      .mapValidated(s =>
        Try(new URI(s)).fold(
          exc => Validated.invalidNel(s"Error converting to URL: ${exc.getMessage}"),
          url => Validated.valid(url)
        )
      )

}
