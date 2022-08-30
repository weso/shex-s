package es.weso.wshex

import es.weso.collection.Bag
import es.weso.rdf.PrefixMap
import cats.implicits._
import java.nio.file.Path
import cats.effect.IO
import es.weso.wbmodel._
import es.weso.utils.VerboseLevel
import es.weso.rdf.nodes._
import java.io.InputStream
import java.io.InputStreamReader
import java.io.ByteArrayInputStream
import es.weso.wshex.compact.Parser._
import es.weso.wshex.compact.ParserOptions

case class WSchema(
    shapesMap: Map[ShapeLabel, WShapeExpr] = Map(),
    start: Option[WShapeExpr] = None,
    prefixes: Option[PrefixMap] = None,
    base: Option[IRI] = None
) extends Serializable {

  def pm: PrefixMap = prefixes.getOrElse(PrefixMap.empty)

  def withPrefixMap(maybePrefixMap: Option[PrefixMap]): WSchema =
    this.copy(prefixes = maybePrefixMap)

  def withStart(start: Option[WShapeExpr]): WSchema = this.copy(start = start)

  def withBase(base: Option[IRI]): WSchema = this.copy(base = base)

  def withShapesMap(sm: Map[ShapeLabel, WShapeExpr]) =
    this.copy(shapesMap = sm)

  def get(shapeLabel: ShapeLabel): Option[WShapeExpr] = shapeLabel match {
    case Start => start
    case _     => shapesMap.get(shapeLabel)
  }

  def checkLocal(label: ShapeLabel, entity: Entity): Either[Reason, Set[ShapeLabel]] =
    get(label) match {
      case None     => Left(ShapeNotFound(label, this))
      case Some(se) => se.checkLocal(entity, label, this)
    }

  def checkLocalCoded(label: ShapeLabel, entity: Entity): Either[ReasonCode, Set[ShapeLabel]] =
    get(label) match {
      case None     => Left(Reason.shapeNotFound)
      case Some(se) => se.checkLocalCoded(entity, label, this)
    }

  def checkNeighs(
      label: ShapeLabel,
      neighs: Bag[(PropertyId, ShapeLabel)],
      failed: Set[(PropertyId, ShapeLabel)]
  ): Either[Reason, Unit] =
    get(label) match {
      case None     => Left(ShapeNotFound(label, this))
      case Some(se) => se.checkNeighs(neighs, failed, this)
    }

  def checkNeighsCoded(
      label: ShapeLabel,
      neighs: Bag[(PropertyId, ShapeLabel)],
      failed: Set[(PropertyId, ShapeLabel)]
  ): Either[ReasonCode, Unit] =
    get(label) match {
      case None     => Left(Reason.shapeNotFound)
      case Some(se) => se.checkNeighsCoded(neighs, failed, this)
    }

  def getTripleConstraints(label: ShapeLabel): List[(PropertyId, ShapeLabel)] =
    get(label) match {
      case None => List()
      case Some(se) =>
        val tcs = se.tripleConstraints(this).map(tc => (tc.property, tc.value.label))
        /* println(s"""|TripleConstraints($label)=
                  |${tcs.mkString("\n")}
                  |---end TripleConstraints($label)
                  |""".stripMargin) */
        tcs
    }

  lazy val shapes: List[WShapeExpr] =
    shapesMap.values.toList

  /** Get a shape with label
    *
    * @param lbl
    * @return the shape expression with that label
    */
  def getShape(lbl: ShapeLabel): Option[WShapeExpr] =
    shapesMap.get(lbl)

  /** Start shape expression in a schema
    *
    * @return the start shape expression if it has been declared or the first one. None if there are no shape expressions
    */
  lazy val startShapeExpr: Option[WShapeExpr] =
    getShape(Start).orElse(shapes.headOption)


}

object WSchema {

  val defaultEntityIRI = es.weso.wbmodel.Value.defaultIRI

  def empty: WSchema =
    WSchema(Map())

  private def cnvFormat(format: WShExFormat): String = format match {
    case WShExFormat.CompactWShExFormat => "ShExC"
    case WShExFormat.ESCompactFormat    => "ShExC"
    case WShExFormat.ESJsonFormat       => "ShExJ"
    case WShExFormat.JsonWShExFormat    => "ShExJ"
  }

  // Technical debt: Unify common code in this method and the next one
  def fromPath(
      path: Path,
      format: WShExFormat = WShExFormat.CompactWShExFormat,
      verbose: VerboseLevel
  ): IO[WSchema] = format match {
    case WShExFormat.CompactWShExFormat | WShExFormat.JsonWShExFormat =>
      for {
        schema <- es.weso.shex.Schema.fromFile(path.toFile().getAbsolutePath(), cnvFormat(format))
        resolvedSchema <- es.weso.shex.ResolvedSchema.resolve(schema, None, verbose)
        schema <- IO.fromEither(ShEx2WShEx().convertSchema(resolvedSchema))
      } yield schema
    case WShExFormat.ESCompactFormat | WShExFormat.ESJsonFormat =>
      for {
        schema <- es.weso.shex.Schema.fromFile(path.toFile().getAbsolutePath(), cnvFormat(format))
        resolvedSchema <- es.weso.shex.ResolvedSchema.resolve(schema, None, verbose)
        wschema <- IO.fromEither(ES2WShEx(ESConvertOptions.default).convertSchema(resolvedSchema))
      } yield wschema
  }

  case class WShExErrorReadingString(msg: String, inputStr: String, format: WShExFormat) extends RuntimeException(msg)

  def fromString(
      schemaString: String,
      format: WShExFormat = WShExFormat.CompactWShExFormat,
      base: Option[IRI] = None,
      entityIRI: IRI = defaultEntityIRI,
      verbose: VerboseLevel
  ): IO[WSchema] = format match {
    case WShExFormat.CompactWShExFormat => {
      val is = new ByteArrayInputStream(schemaString.getBytes())
      val reader = new InputStreamReader(is)
      val parserOptions = ParserOptions(entityIRI)
      parseSchemaReader(reader, base, parserOptions).fold(e => IO.raiseError(WShExErrorReadingString(e, schemaString, format)), _.pure[IO])
    }
    case WShExFormat.JsonWShExFormat =>
      for {
        schema <- es.weso.shex.Schema.fromString(schemaString, cnvFormat(format))
        resolvedSchema <- es.weso.shex.ResolvedSchema.resolve(schema, None, verbose)
        wschema <- IO.fromEither(ShEx2WShEx().convertSchema(resolvedSchema))
      } yield wschema
    case WShExFormat.ESCompactFormat | WShExFormat.ESJsonFormat =>
      for {
        schema <- es.weso.shex.Schema.fromString(schemaString, cnvFormat(format))
        resolvedSchema <- es.weso.shex.ResolvedSchema.resolve(schema, None, verbose)
        wschema <- IO.fromEither(ES2WShEx(ESConvertOptions.default).convertSchema(resolvedSchema))
      } yield wschema
  }

  /** Read a Schema from a file
    * This version is unsafe in the sense that it can throw exceptions
    * Use `fromPath` for a safe version which returns an `IO[Schema]`
    *
    * @param path file to read
    * @param format it can be CompactFormat or JsonFormat
    * @return the schema
    */
  def unsafeFromPath(
      path: Path,
      format: WShExFormat = WShExFormat.CompactWShExFormat,
      verbose: VerboseLevel
  ): WSchema = {
    import cats.effect.unsafe.implicits.global
    fromPath(path, format, verbose).unsafeRunSync()
  }

  /** Read a Schema from a file
    * This version is unsafe in the sense that it can throw exceptions
    * Use `fromPath` for a safe version which returns an `IO[Schema]`
    *
    * @param str string that represents the schema
    * @param format schema format
    * @return the schema
    */
  def unsafeFromString(
      str: String,
      format: WShExFormat,
      base: Option[IRI] = None,
      entityIRI: IRI = defaultEntityIRI, 
      verbose: VerboseLevel
  ): Either[ParseError, WSchema] = {
    import cats.effect.unsafe.implicits.global
    try
      /*      val schema = es.weso.shex.Schema.fromString(str, cnvFormat(format)).unsafeRunSync()
      val wShEx = ShEx2WShEx().convertSchema(schema) */
      fromString(str, format, base, entityIRI, verbose).unsafeRunSync().asRight
    catch {
      case e: Exception => ParseException(e).asLeft
    }
  }

  /** Read a Schema from a file
    * This version is unsafe in the sense that it can throw exceptions
    * Use `fromPath` for a safe version which returns an `IO[Schema]`
    *
    * @param str String that represents the schema
    * @param format it can be CompactFormat or JsonFormat
    * @return the schema
    */
  def unsafeFromString2(
      schemaString: String,
      format: WShExFormat = WShExFormat.CompactWShExFormat,
      base: Option[IRI] = None,
      entityIRI: IRI = defaultEntityIRI,
      verbose: VerboseLevel
  ): WSchema = {
    import cats.effect.unsafe.implicits.global
    fromString(schemaString, format, base, entityIRI, verbose).unsafeRunSync()
  }
}
