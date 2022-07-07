package es.weso.shapepath.schemamappings

import java.io.{ByteArrayInputStream, InputStreamReader, Reader => JavaReader}

import cats.data._
import cats.implicits._
// import com.typesafe.scalalogging._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.shex._
import org.antlr.v4.runtime._
import java.nio.charset.StandardCharsets

import es.weso.shapepath.ShapePath
import es.weso.shapepath.parser.{ShapePathDocLexer, ShapePathDocParser}
import es.weso.utils.FileUtils

import scala.collection.immutable.ListMap
import es.weso.shapepath.parser.SchemaMappingsDocParser
import es.weso.shapepath.parser.SchemaMappingsDocLexer

object SchemaMappingsParser {

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

  def getShapesMap: Builder[ShapesMap] =
    getState.map(_.shapesMap)

  def getTripleExprMap: Builder[TripleExprMap] =
    getState.map(_.tripleExprMap)

  def getState: Builder[BuilderState] =
    EitherT.liftF[S, String, BuilderState](StateT.inspect(identity))

  def getBase: Builder[Option[IRI]] =
    getState.map(_.base)

  def getStart: Builder[Start] =
    getState.map(_.start)

  def addBase(base: IRI): Builder[Unit] =
    updateState(_.copy(base = Some(base)))

  def updateState(fn: BuilderState => BuilderState): Builder[Unit] =
    EitherT.liftF[S, String, Unit](StateT.modify(fn))

  def updateStart(s: Start): Builder[Unit] =
    // logger.info(s"New start: $s")
    updateState(_.copy(start = s))

  // TODO: Check what to do if the label is already assigned
  def addShape(label: ShapeLabel, expr: ShapeExpr): Builder[Unit] =
    updateState(s => s.copy(shapesMap = s.shapesMap + (label -> expr)))

  def addPrefix(prefix: Prefix, iri: IRI): Builder[Unit] =
    updateState { s =>
      val newS = s.copy(prefixMap = s.prefixMap.addPrefix(prefix, iri))
      // logger.info(s"Updating prefix map. New prefix map=${newS.prefixMap}")
      newS
    }

  def addTripleExprLabel(label: ShapeLabel, te: TripleExpr): Builder[TripleExpr] = for {
    s <- getState
    _ <- s.tripleExprMap.get(label) match {
      case None => ok(())
      case Some(otherTe) =>
        err(s"Label $label has been assigned to ${otherTe} and can't be assigned to $te")
    }
    _ <- updateState(s => s.copy(tripleExprMap = s.tripleExprMap + (label -> te)))
  } yield te.addId(label)

  def parseSchemaMappings(str: String, base: Option[IRI]): Either[String, SchemaMappings] = {
    val UTF8_BOM = "\uFEFF"
    val s =
      if (str.startsWith(UTF8_BOM)) {
        // logger.debug("BOM detected and removed")
        str.substring(1)
      } else str
    val reader: JavaReader =
      new InputStreamReader(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    // logger.debug(s"s:$s")
    parseReader(reader, base)
  }

  /*  def parseShapePathFromFile(fileName: String, base: Option[IRI]): Either[String, ShapePath] = for {
    reader <- FileUtils.getStream(fileName)
    schema <- parseReader(reader, base)
  } yield schema */

  def parseReader(reader: JavaReader, base: Option[IRI]): Either[String, SchemaMappings] = {
    val input: CharStream = CharStreams.fromReader(reader)
    val lexer: SchemaMappingsDocLexer = new SchemaMappingsDocLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: SchemaMappingsDocParser = new SchemaMappingsDocParser(tokens)

    val errorListener = new ParserErrorListener
    lexer.addErrorListener(errorListener)
    parser.addErrorListener(errorListener)

    val maker = new SchemaMappingsMaker()
    val builder: Builder[SchemaMappings] =
      maker.visit(parser.schemaMappingsDoc()).asInstanceOf[Builder[SchemaMappings]]
    val errors = errorListener.getErrors()
    if (errors.length > 0) {
      Left(errors.mkString("\n"))
    } else {
      run(builder, base)._2
    }
  }

  def run[A](c: Builder[A], base: Option[IRI]): (BuilderState, Either[String, A]) =
    c.value.run(initialState(base)).value

  def initialState(base: Option[IRI]) =
    BuilderState(PrefixMap.empty, base, None, ListMap(), Map())

  case class BuilderState(
      prefixMap: PrefixMap,
      base: Option[IRI],
      start: Option[ShapeExpr],
      shapesMap: ShapesMap,
      tripleExprMap: TripleExprMap
  )

}
