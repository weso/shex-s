package es.weso.wshex.compact

import java.io.{ByteArrayInputStream, InputStreamReader, Reader => JavaReader}
import cats.data._
import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.wshex._
import es.weso.wshex.parser._
import org.antlr.v4.runtime._
import java.nio.charset.StandardCharsets
import es.weso.utils.FileUtils
import scala.collection.immutable.ListMap
import es.weso.rdf.locations.Location

object Parser {

  case class BuilderState(
      prefixMap: PrefixMap,
      base: Option[IRI],
      start: Option[WShapeExpr],
      shapesMap: ShapesMap,
      // tripleExprMap: TripleExprMap,
      labelLocationMap: Map[ShapeLabel, Location]
  )

  type S[A] = State[BuilderState, A]
  type Builder[A] = EitherT[S, String, A]

  // type PrefixMap = Map[Prefix,IRI]
  type Start = Option[WShapeExpr]
  type ShapesMap = ListMap[ShapeLabel, WShapeExpr]
  type TripleExprMap = Map[ShapeLabel, TripleExpr]

  def ok[A](x: A): Builder[A] =
    EitherT.pure(x)

  def err[A](msg: String): Builder[A] = {
    val r: S[String] = StateT.pure(msg)
    val v: Builder[A] = EitherT.left[A](r)
    v
  }

  def info(msg: String): Builder[Unit] = {
    println(msg)
    EitherT.pure(())
  }

  def fromEither[A](e: Either[String, A]): Builder[A] =
    e.fold(str => err(str), ok(_))

  def sequence[A](bs: List[Builder[A]]): Builder[List[A]] =
    bs.sequence[Builder, A]

  def getPrefixMap: Builder[PrefixMap] =
    getState.map(_.prefixMap)

  def getShapesMap: Builder[ShapesMap] =
    getState.map(_.shapesMap)

/*  def getTripleExprMap: Builder[TripleExprMap] =
    getState.map(_.tripleExprMap) */

  def getLabelLocationMap: Builder[Map[ShapeLabel, Location]] =
    getState.map(_.labelLocationMap)

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
  def addShape(label: ShapeLabel, expr: WShapeExpr): Builder[Unit] =
    updateState(s => s.copy(shapesMap = s.shapesMap + (label -> expr)))

  def addLabelLocation(label: ShapeLabel, location: Location): Builder[Unit] =
    updateState(s => s.copy(labelLocationMap = s.labelLocationMap + (label -> location)))

  def addPrefix(prefix: Prefix, iri: IRI): Builder[Unit] =
    updateState { s =>
      val newS = s.copy(prefixMap = s.prefixMap.addPrefix(prefix, iri))
      // logger.info(s"Updating prefix map. New prefix map=${newS.prefixMap}")
      newS
    }

/*  def addTripleExprLabel(label: ShapeLabel, te: TripleExpr): Builder[TripleExpr] = for {
    s <- getState
    _ <- s.tripleExprMap.get(label) match {
      case None => ok(())
      case Some(otherTe) =>
        err(s"Label $label has been assigned to ${otherTe} and can't be assigned to $te")
    }
    _ <- updateState(s => s.copy(tripleExprMap = s.tripleExprMap + (label -> te)))
  } yield te.addId(label) */

  def parseSchema(str: String, base: Option[IRI]): Either[String, WSchema] = {
    val UTF8_BOM = "\uFEFF"
    val s =
      if (str.startsWith(UTF8_BOM)) {
        // logger.debug("BOM detected and removed")
        str.substring(1)
      } else str
    val reader: JavaReader =
      new InputStreamReader(new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8)))
    // logger.debug(s"s:$s")
    parseSchemaReader(reader, base)
  }

  def parseSchemaFromFile(fileName: String, base: Option[IRI]): Either[String, WSchema] = for {
    reader <- FileUtils.getStream(fileName)
    schema <- parseSchemaReader(reader, base)
  } yield schema

  def parseSchemaReader(reader: JavaReader, base: Option[IRI]): Either[String, WSchema] = {
    val input: CharStream = CharStreams.fromReader(reader)
    val lexer: WShExDocLexer = new WShExDocLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lexer)
    val parser: WShExDocParser = new WShExDocParser(tokens)

    val errorListener = new ParserErrorListener
    // lexer.removeErrorListeners()
    // parser.removeErrorListeners()
    lexer.addErrorListener(errorListener)
    parser.addErrorListener(errorListener)

    val maker = new SchemaMaker() // new DebugSchemaMaker()
    val builder = maker.visit(parser.wShExDoc()).asInstanceOf[Builder[WSchema]]
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
    BuilderState(
      PrefixMap.empty,
      base,
      None,
      ListMap(),
      Map()
    )


}
