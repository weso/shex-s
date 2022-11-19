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
import java.io.FileInputStream
import java.nio.file.Files
import es2wshex._

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

  lazy val labels: Set[ShapeLabel] = shapesMap.keys.toSet

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
    start.orElse(shapes.headOption)

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

  def parseFormat(formatStr: String): IO[WShExFormat] =
    IO.fromEither(WShExFormat.fromString(formatStr))

  def fromInputStream(
      is: InputStream,
      format: WShExFormat = WShExFormat.CompactWShExFormat,
      base: Option[IRI] = None,
      entityIRI: IRI = defaultEntityIRI,
      verbose: VerboseLevel
  ): IO[WSchema] = format match {
    case WShExFormat.CompactWShExFormat =>
      val reader = new InputStreamReader(is)
      val parserOptions = ParserOptions(entityIRI)
      parseSchemaReader(reader, base, parserOptions)
        .fold(e => IO.raiseError(WShExErrorReading(e, format)), _.pure[IO])
    case WShExFormat.ESCompactFormat | WShExFormat.ESJsonFormat =>
      for {
        schema <- es.weso.shex.Schema.fromInputStream(is, cnvFormat(format), base)
        resolvedSchema <- es.weso.shex.ResolvedSchema.resolve(schema, base, verbose)
        wschema <- IO.fromEither(
          ES2WShEx(ES2WShExConvertOptions.default).convert(resolvedSchema)
        )
      } yield wschema
    case _ => IO.raiseError(WShExUnsupportedFormat(format))
  }

  def fromPath(
      path: Path,
      format: WShExFormat = WShExFormat.CompactWShExFormat,
      base: Option[IRI] = None,
      entityIRI: IRI = defaultEntityIRI,
      verbose: VerboseLevel
  ): IO[WSchema] =
    fromInputStream(Files.newInputStream(path), format, base, entityIRI, verbose)

  def fromString(
      schemaString: String,
      format: WShExFormat = WShExFormat.CompactWShExFormat,
      base: Option[IRI] = None,
      entityIRI: IRI = defaultEntityIRI,
      verbose: VerboseLevel
  ): IO[WSchema] = {
    val is = new ByteArrayInputStream(schemaString.getBytes())
    fromInputStream(is, format, base, entityIRI, verbose)
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
      base: Option[IRI] = None,
      entityIRI: IRI = defaultEntityIRI,
      verbose: VerboseLevel
  ): WSchema = {
    import cats.effect.unsafe.implicits.global
    fromPath(path, format, base, entityIRI, verbose).unsafeRunSync()
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
      fromString(str, format, base, entityIRI, verbose).unsafeRunSync().asRight
    catch {
      case e: Exception => ParseException(e).asLeft
    }
  }

  case class WShExErrorReadingString(msg: String, inputStr: String, format: WShExFormat)
      extends RuntimeException(s"""|Error reading WSchema from String
                         |Error: $msg
                         |String: ${inputStr}
                         |""".stripMargin)
  case class WShExErrorReadingPath(msg: String, inputPath: Path, format: WShExFormat)
      extends RuntimeException(s"""|Error reading WSchema from path
                         |Error: $msg
                         |Path: $inputPath
                         |""".stripMargin)
  case class WShExErrorReading(msg: String, format: WShExFormat)
      extends RuntimeException(s"""|Error reading WSchema from path
                         |Error: $msg
                         |""".stripMargin)
  case class WShExUnsupportedFormat(format: WShExFormat)
      extends RuntimeException(s"""|Error reading WSchema.
                         |Unsupported format yet: $format
                         |""".stripMargin)

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
