package es.weso.shexs

import org.rogach.scallop._
import es.weso.shapeMaps.ShapeMapFormat



class MainOpts(arguments: Array[String], 
               onError: (Throwable, Scallop) => Nothing) extends ScallopConf(arguments) {

  banner("""| shex: ShEx-s processor
            | Options:
            | """.stripMargin)


  footer("Enjoy!")

  val manifest: ScallopOption[String] = opt[String](
    name = "manifest",
    default = None,
    descr = "Manifest file to test",
    noshort = true)

  val shapeMap: ScallopOption[String] = opt[String](
    name = "shapeMap",
    descr = "ShapeMap",
    short = 'm')

  val schema: ScallopOption[String] = opt[String](
    name = "schema",
    descr = "Schema",
    short = 's')

  val schemaFormat: ScallopOption[String] = opt[String](
    name = "schemaFormat",
    descr = "Schema",
    default = Some("ShExC"),
    noshort = true)

  val data: ScallopOption[String] = opt[String](
    name = "data",
    descr = "Data",
    short = 'd')

  val showData: ScallopOption[Boolean] = opt[Boolean](
    name = "showData",
    descr = "Show RDF data",
    default = Some(false),
    noshort = true)

  val showDataFormat: ScallopOption[String] = opt[String](
    name = "showDataFormat",
    descr = "Format to show RDF data",
    default = Some("Turtle"),
    noshort = true
  )

  val showSchema: ScallopOption[Boolean] = opt[Boolean](
    name = "showSchema",
    descr = "Show Schema",
    default = Some(false),
    noshort = true
  )

  val showSchemaFormat: ScallopOption[String] = opt[String](
    "showSchemaFormat",
    default = Some("ShExC"),
    descr = "Format to show Schema",
    noshort = true
  )

  val showResultFormat: ScallopOption[String] = opt[String](
    "showResultFormat",
    default = Some("Compact"),
    descr = s"Format to show Result. Available formats: ${ShapeMapFormat.availableFormatNames.mkString(",")}",
    noshort = true
  )

  val schemaFile: ScallopOption[String] = opt[String](
    name = "schemaFile",
    descr = "Schema file",
    default = None,
    noshort = true
  )

  val dataFormat: ScallopOption[String] = opt[String](
    name = "dataFormat",
    descr = "Data format",
    default = Some("Turtle"),
    noshort = true
  )

  val dataFile: ScallopOption[String] = opt[String](
    name = "dataFile",
    descr = "Data file",
    default = None,
    noshort = true
  )


}
