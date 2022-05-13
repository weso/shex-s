package es.weso.shapepath.compact

import java.io.{ByteArrayInputStream, InputStreamReader, Reader => JavaReader}

import cats.data._
import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.shex._
import org.antlr.v4.runtime._
import java.nio.charset.StandardCharsets

import es.weso.shapepath.ShapePath
import es.weso.shapepath.parser.{ShapePathDocLexer, ShapePathDocParser}
import es.weso.utils.FileUtils

import scala.collection.immutable.ListMap

object Parser {

  type S[A] = State[BuilderState, A]
  type Builder[A] = EitherT[S, String, A]

  // type PrefixMap = Map[Prefix,IRI]
  type Start = Option[ShapeExpr]
  type ShapesMap = ListMap[ShapeLabel, ShapeExpr]
  type TripleExprMap = Map[ShapeLabel, TripleExpr]

  def ok[A](x: A): Builder[A] =
    EitherT.pure(x)

  def err[A](msg: String): Builder[A] = {
    val r: S[String] = StateT.pure(msg)
    val v: Builder[A] = EitherT.left[A](r)
    v
  }

  def fromEither[A](e: Either[String, A]): Builder[A] =
    e.fold(str => err(str), ok(_))

  def sequence[A](bs: List[Builder[A]]): Builder[List[A]] =
    bs.sequence[Builder, A]

  def getPrefixMap: Builder[PrefixMap] =
    getState.map(_.prefixMap)

  def getState: Builder[BuilderState] =
    EitherT.liftF[S, String, BuilderState](StateT.inspect(identity))

  def getBase: Builder[Option[IRI]] =
    getState.map(_.base)

  def addBase(base: IRI): Builder[Unit] =
    updateState(_.copy(base = Some(base)))

  def updateState(fn: BuilderState => BuilderState): Builder[Unit] =
    EitherT.liftF[S, String, Unit](StateT.modify(fn))

  def addPrefix(prefix: Prefix, iri: IRI): Builder[Unit] =
    updateState { s =>
      val newS = s.copy(prefixMap = s.prefixMap.addPrefix(prefix, iri))
      // logger.info(s"Updating prefix map. New prefix map=${newS.prefixMap}")
      newS
    }

  def parseShapePath(
      str: String,
      base: Option[IRI],
      prefixMap: PrefixMap
  ): Either[String, ShapePath] = {
    val UTF8_BOM = "\uFEFF"
    val s =
      if (str.startsWith(UTF8_BOM)) {
        // logger.debug("BOM detected and removed")
        str.substring(1)
      } else str
    val reader: JavaReader =
      new InputStreamReader(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    // logger.debug(s"s:$s")
    parseReader(reader, base, prefixMap)
  }

  def parseShapePathFromFile(
      fileName: String,
      base: Option[IRI],
      prefixMap: PrefixMap
  ): Either[String, ShapePath] = for {
    reader <- FileUtils.getStream(fileName)
    schema <- parseReader(reader, base, prefixMap)
  } yield schema

  def parseReader(
      reader: JavaReader,
      base: Option[IRI],
      prefixMap: PrefixMap
  ): Either[String, ShapePath] = {
    val input: CharStream = CharStreams.fromReader(reader)
    val lexer: ShapePathDocLexer = new ShapePathDocLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: ShapePathDocParser = new ShapePathDocParser(tokens)

    val errorListener = new ParserErrorListener
    // lexer.removeErrorListeners()
    // parser.removeErrorListeners()
    lexer.addErrorListener(errorListener)
    parser.addErrorListener(errorListener)

    val maker = new ShapePathMaker
    val builder = maker.visit(parser.shapePathDoc()).asInstanceOf[Builder[ShapePath]]
    val errors = errorListener.getErrors()
    if (errors.length > 0) {
      Left(errors.mkString("\n"))
    } else {
      run(builder, base, prefixMap)._2
    }
  }

  def run[A](
      c: Builder[A],
      base: Option[IRI],
      prefixMap: PrefixMap
  ): (BuilderState, Either[String, A]) =
    c.value.run(initialState(base, prefixMap)).value

  def initialState(base: Option[IRI], prefixMap: PrefixMap) =
    BuilderState(prefixMap, base)

  case class BuilderState(prefixMap: PrefixMap, base: Option[IRI])

}
