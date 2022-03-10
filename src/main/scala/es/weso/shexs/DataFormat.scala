package es.weso.shexs

import com.monovore.decline.Opts

object DataFormat {

  lazy val availableDataFormats    = List("Turtle", "NTriples", "RDF/XML", "JSON-LD")
  lazy val defaultDataFormat       = availableDataFormats.head
  lazy val availableDataFormatsStr = availableDataFormats.mkString(",")

  lazy val dataFormatOpt = Opts
    .option[String]("dataFormat", help = s"Data format. Default=$defaultDataFormat, available=$availableDataFormatsStr")
    .withDefault(defaultDataFormat)

}
