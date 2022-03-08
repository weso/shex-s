package es.weso.shapemaps

import java.io.{ByteArrayInputStream, InputStreamReader, Reader => JavaReader}
import java.nio.charset.StandardCharsets

import es.weso.rdf._
import es.weso.rdf.nodes.IRI
import es.weso.shapemaps.parser.{ShapeMapLexer, ShapeMapParser}
import org.antlr.v4.runtime._
import cats.data._
import cats.implicits._

object Parser  {

  type Builder[A] = Either[String, A]

  def ok[A](x: A): Builder[A] = Right(x)

  def err[A](msg: String): Builder[A] = Left(msg)

  def removeBOM(str: String): String = {
    val UTF8_BOM = "\uFEFF"
    if (str.startsWith(UTF8_BOM)) {
      // logger.info("BOM detected and removed")
      str.substring(1)
    } else {
      str
    }
  }

  def parse(
    str: String,
    base: Option[IRI],
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap): Either[NonEmptyList[String], QueryShapeMap] = {
    val s = removeBOM(str)
    val reader: JavaReader =
      new InputStreamReader(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    parseReader(reader, base, nodesPrefixMap, shapesPrefixMap)
  }

  def parseReader(
    reader: JavaReader,
    base: Option[IRI],
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap): Either[NonEmptyList[String], QueryShapeMap] = {
    val input: CharStream = CharStreams.fromReader(reader)
    val lexer: ShapeMapLexer = new ShapeMapLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: ShapeMapParser = new ShapeMapParser(tokens)

    val errorListener = new ParserErrorListener
    lexer.addErrorListener(errorListener)
    parser.addErrorListener(errorListener)

    val maker = new ShapeMapsMaker(base,nodesPrefixMap,shapesPrefixMap)
    val builder = maker.visit(parser.shapeMap()).asInstanceOf[Builder[QueryShapeMap]]
    val errors = errorListener.getErrors
    NonEmptyList.fromList(errors).fold(
      builder.leftMap(NonEmptyList.one(_)))(
      _.asLeft
    )
  }

}
