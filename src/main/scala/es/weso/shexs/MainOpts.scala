package es.weso.shexs

import org.rogach.scallop._

class MainOpts(arguments: Array[String], 
               onError: (Throwable, Scallop) => Nothing) extends ScallopConf(arguments) {

  banner("""| shaclex: ShEx-s processor
                | Options:
                |""".stripMargin)


  footer("Enjoy!")

  val manifest: ScallopOption[String] = opt[String](
    "manifest",
    default = None,
    descr = "Manifest file to test",
    short = 'm')

}
