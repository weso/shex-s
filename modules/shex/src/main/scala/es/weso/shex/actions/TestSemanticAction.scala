package es.weso.shex.actions
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{IRI, RDFNode}
import cats.effect.IO
import es.weso.shex.validator.ShExError.FailSemanticAction

object TestSemanticAction {
  val processorName = "Test"
  val iri = IRI("http://shex.io/extensions/Test/")

  def runAction(code: String, node: RDFNode, rdf: RDFReader): IO[Unit] = {
    val printExpr = "print\\((.*)\\)".r
    val failExpr = "fail\\((.*)\\)".r
    val sExpr = raw"s".r
    val cleanedStr = code.stripPrefix(" ").stripSuffix(" ")
    cleanedStr match {
      case printExpr(str) =>
        str match {
          case sExpr() => IO(println(s"$node"))
          case _       => IO(println(str))
        }
      case failExpr(str) =>
        val s = str match {
          case sExpr() => s"$node"
          case _       => str
        }
        IO.raiseError(FailSemanticAction(node, s"Error: $s. Processor: $processorName"))
      case "" => IO.unit
      case str =>
        IO.raiseError(FailSemanticAction(node, s"Unknown command: $str. Processor: $processorName"))
    }
  }
}
