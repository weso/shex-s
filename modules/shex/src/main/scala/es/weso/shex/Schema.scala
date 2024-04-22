package es.weso.shex

//import java.io.File
import java.nio.file.{Files, Paths}

import cats.implicits._
import es.weso.depgraphs.DepGraph
import es.weso.rdf.{PrefixMap, RDFBuilder, RDFReader}
import es.weso.rdf.nodes._
import es.weso.shex.shexR.{RDF2ShEx, ShEx2RDF}
import scala.io.Source
import scala.util._
import cats.effect._
import es.weso.utils.FileUtils
import es.weso.rdf.locations.Location
import compact.Parser._
import java.io.InputStream
import java.io.InputStreamReader
import java.io.ByteArrayInputStream
import java.nio.file.{Path => FilePath}
import es.weso.utils.VerboseLevel

case class Schema(
    id: IRI,
    prefixes: Option[PrefixMap],
    base: Option[IRI],
    startActs: Option[List[SemAct]],
    start: Option[ShapeExpr],

    // TODO: Replace this by a map?
    shapes: Option[List[ShapeExpr]],
    optTripleExprMap: Option[Map[ShapeLabel, TripleExpr]],
    imports: List[IRI],
    labelLocationMap: Option[Map[ShapeLabel, Location]]
) extends AbstractSchema {

  def addShape(se: ShapeExpr): Schema =
    this.copy(shapes = shapes match {
      case None     => Some(List(se))
      case Some(ls) => Some(se :: removeShapeWithLabel(se.id, ls))
    })

  private def removeShapeWithLabel(
      maybeLbl: Option[ShapeLabel],
      ls: List[ShapeExpr]
  ): List[ShapeExpr] =
    maybeLbl match {
      case None => ls
      case Some(lbl) =>
        ls.filter(se => se.id.map(_ != lbl).getOrElse(true))
    }

  def getTripleExpr(lbl: ShapeLabel): Either[String, TripleExpr] =
    optTripleExprMap match {
      case None    => Left(s"Cannot find $lbl with an empty tripleExpr map")
      case Some(m) => m.get(lbl).toRight(s"Not found $lbl in map $m")
    }

  def shapeLabel2Iri(l: ShapeLabel): Either[String, IRI] = l match {
    case l: IRILabel => Right(l.iri)
    case _           => Left(s"Label $l can't be converted to IRI")
  }

  def addId(i: IRI): Schema = this.copy(id = i)

  def withPrefixMap(maybePrefixMap: Option[PrefixMap]): Schema =
    this.copy(prefixes = maybePrefixMap)

  def withStartActions(maybeStartActions: Option[List[SemAct]]): Schema =
    this.copy(startActs = maybeStartActions)

  def withStart(start: Option[ShapeExpr]): Schema = this.copy(start = start)

  def withBase(base: Option[IRI]): Schema = this.copy(base = base)

  def withShapes(shapes: Option[List[ShapeExpr]]): Schema = this.copy(shapes = shapes)

  def withImports(imports: List[IRI]): Schema = this.copy(imports = imports)

  def withOptTripleExprMap(optTripleExprMap: Option[Map[ShapeLabel, TripleExpr]]): Schema =
    this.copy(optTripleExprMap = optTripleExprMap)

  def withLabelLocationMap(labelLocationMap: Option[Map[ShapeLabel, Location]]): Schema =
    this.copy(labelLocationMap = labelLocationMap)

  def getShape(label: ShapeLabel): Either[String, ShapeExpr] = for {
    se <- shapesMap.get(label) match {
      case None =>
        s"Not found $label in schema. Available labels: ${shapesMap.keySet.mkString}"
          .asLeft[ShapeExpr]
      case Some(se) => se.asRight[String]
    }
  } yield se

  lazy val localShapes: List[ShapeExpr] = shapes.getOrElse(List())

  lazy val shapeList: List[ShapeExpr] = shapes.getOrElse(List())

  override def labels: List[ShapeLabel] = localShapes.map(_.id).flatten

  def addTripleExprMap(te: Map[ShapeLabel, TripleExpr]): Schema =
    this.copy(optTripleExprMap = Some(te))

  def oddNegCycles: Either[String, Set[Set[(ShapeLabel, ShapeLabel)]]] =
    Dependencies.oddNegCycles(this)

  def negCycles: Either[String, Set[Set[(ShapeLabel, ShapeLabel)]]] =
    Dependencies.negCycles(this)

  def depGraph: Either[String, DepGraph[ShapeLabel]] =
    Dependencies.depGraph(this)

  def showCycles(str: Either[String, Set[Set[(ShapeLabel, ShapeLabel)]]]): String = str match {
    case Left(e)   => e
    case Right(ss) => ss.map(s => s.map(_.toString).mkString(",")).mkString("\n")
  }

  private def checkShapeLabel(lbl: ShapeLabel): Either[String, Unit] = for {
    se <- getShape(lbl)
    // _ <- { println(s"Label ${lbl.toString}. ShapeExpr: ${se.toString}"); Right(())}
    refs <- se.getShapeRefs(this)
    // _ <- { println(s"Refs ${refs.toString}"); Right(())}
  } yield
  // println(s"Label: $lbl, refs: ${se.getShapeRefs(this).mkString(",")}")
  // println(s"References: ${refs.mkString(",")}")
  ()

  private lazy val checkBadShapeLabels: Either[String, Unit] = for {
    // _ <- { println(s"shapesMap: $shapesMap"); Right(())}
    _ <- shapesMap.keySet.toList.map(lbl => checkShapeLabel(lbl)).sequence
  } yield (())

  private lazy val checkOddNegCycles: Either[String, Unit] =
    // println(s"OddNegCycles: $oddNegCycles")
    oddNegCycles match {
      case Left(e) => Left(e)
      case Right(cs) =>
        if (cs.isEmpty) Right(())
        else
          Left(s"Negative cycles: ${showCycles(oddNegCycles)}")
    }

  lazy val wellFormed: Either[String, Unit] = for {
    _ <- checkOddNegCycles
    // _ <- { println(s"Passed checkOddNegCycles..."); Right(())}
    _ <- checkBadShapeLabels
    // _ <- { println(s"Passed checkBadShapeLabels..."); Right(())}
  } yield (())

  def relativize(maybeBase: Option[IRI]): Schema = maybeBase match {
    case None => this
    case Some(baseIri) =>
      this.copy(
        id = id.relativizeIRI(baseIri),
        base = base.map(_.relativizeIRI(baseIri)),
        start = start.map(_.relativize(baseIri)),
        shapes = shapes.map(_.map(_.relativize(baseIri)))
      )
  }

  def resolve(base: Option[IRI], verbose: VerboseLevel): IO[ResolvedSchema] =
    ResolvedSchema.resolve(this, base, verbose)

  def withId(iri: IRI): Schema = this.copy(id = iri)

  def withShapes(ses: ShapeExpr*): Schema = this.copy(shapes = ses.toList.some)

}

object Schema {

  def rdfDataFormats(rdfReader: RDFReader) = rdfReader.availableParseFormats.map(_.toUpperCase)

  def empty: Schema =
    Schema(IRI(""), None, None, None, None, None, None, List(), None)

  def emptyWithId(iri: IRI): Schema =
    Schema.empty.withId(iri)

  def fromIRI(
      i: IRI,
      base: Option[IRI],
      verbose: VerboseLevel,
      assumeLocal: Option[(IRI, FilePath)] = None
  ): IO[Schema] = {
    val uri = i.uri
    // println(s"Schema.fromIRI. $assumeLocal")
    if (uri.getScheme == "file") {
      if (Files.exists(Paths.get(i.uri))) {
        val str = Source.fromURI(uri).mkString
        fromString(str, "ShExC", Some(i)).map(schema => schema.addId(i))
      } else {
        val iriShEx = i + ".shex"
        if (Files.exists(Paths.get(iriShEx.uri))) {
          val str = Source.fromURI(iriShEx.uri).mkString
          fromString(str, "ShExC", Some(i)).map(schema => schema.addId(i))
        } else {
          val iriJson = i + ".json"
          if (Files.exists(Paths.get(iriJson.uri))) {
            val str = Source.fromURI(iriJson.uri).mkString
            fromString(str, "JSON", Some(i)).map(schema => schema.addId(i))
          } else {
            // println(s"File $i does not exist")
            err(s"File $i does not exist")
          }
        }
      }
    } else
      for {
        schema <- getSchemaWithExts(
          i,
          List(("", "ShExC"), ("shex", "ShExC"), ("json", "JSON")),
          base,
          verbose
        )
      } yield schema.addId(i)
  }

  private def getSchemaWithExts(
      iri: IRI,
      exts: List[(String, String)],
      base: Option[IRI],
      verbose: VerboseLevel
  ): IO[Schema] =
    exts match {
      case (e :: es) =>
        getSchemaExt(iri, e, base, verbose).orElse(getSchemaWithExts(iri, es, base, verbose))
      case Nil => err(s"Can not obtain schema from iri: $iri, Exts: ${exts} ")
    }

  private def getSchemaExt(
      iri: IRI,
      pair: (String, String),
      base: Option[IRI],
      verbose: VerboseLevel
  ): IO[Schema] = {
    val (ext, format) = pair
    val uri =
      if (ext == "") iri.uri
      else (iri + "." + ext).uri
    for {
      _ <- verbose.info(s"""|getSchemaExt(iri: $iri, ext: $ext, format: $format
                           |Effective uri to deref:$uri""".stripMargin)
      str <- derefUri(uri, verbose)
      _ <- verbose.debug(s"Str obtained\n${str.linesIterator.take(2).mkString("\n")}")
      schema <- Schema.fromString(str, format, base, None)
      _ <- verbose.details(s"Obtained schema at $uri\n${schema}")
    } yield schema
  }

  /** Reads a Schema from a Reader
    * @param is input stream
    * @param format syntax format
    * @param base base URL
    * @param maybeRDFBuilder RDFReader value from which to obtain RDF data formats (in case of RDF format)
    * @return either a Schema or a String message error
    */
  def fromInputStream(
      is: InputStream,
      format: String = "ShExC",
      base: Option[IRI] = None,
      maybeRDFBuilder: Option[RDFBuilder] = None
  ): IO[Schema] = {
    val formatUpperCase = format.toUpperCase
    formatUpperCase match {
      case "SHEXC" =>
        val reader = new InputStreamReader(is)
        parseSchemaReader(reader, base).fold(e => err(e), ok)
      // TODO: The following code loads the inputstream in memory...
      // look for streaming alternatives!
      case "SHEXJ" =>
        import io.circe.parser._
        import es.weso.shex.implicits.decoderShEx._
        getContents(is).flatMap(str =>
          decode[Schema](str).leftMap(_.getMessage).fold(e => err(e), ok)
        )
      case _ =>
        maybeRDFBuilder match {
          case None =>
            err(s"Not implemented ShEx parser for format $format and no rdfReader provided")
          case Some(rdfBuilder) =>
            if (rdfDataFormats(rdfBuilder).contains(formatUpperCase)) {
              getContents(is).flatMap(str =>
                rdfBuilder
                  .fromString(str, formatUpperCase, base)
                  .flatMap(
                    _.use(rdf =>
                      for {
                        eitherSchema <- RDF2ShEx.rdf2Schema(rdf)
                        schema <- eitherSchema.fold(e => err(e), ok)
                      } yield schema
                    )
                  )
              )
            } else err(s"Not implemented ShEx parser for format $format")
        }
    }
  }

  private def err[A](msg: String): IO[A] = IO.raiseError(new RuntimeException(msg))
  private def ok[A](v: A): IO[A] = IO.pure(v)

  def serialize(
      schema: Schema,
      format: String,
      base: Option[IRI],
      rdfBuilder: RDFBuilder
  ): IO[String] = {
    val formatUpperCase = format.toUpperCase
    val relativeSchema = schema.relativize(base)
    formatUpperCase match {
      case "SHEXC" =>
        import compact.CompactShow._
        IO.pure(showSchema(relativeSchema))
      case "SHEXJ" =>
        import io.circe.syntax._
        import es.weso.shex.implicits.encoderShEx._
        IO.pure(relativeSchema.asJson.spaces2)
      case _ if rdfDataFormats(rdfBuilder).contains(formatUpperCase) =>
        rdfBuilder.empty.flatMap(
          _.use(empty =>
            for {
              rdf <- ShEx2RDF(relativeSchema, None, empty)
              str <- rdf.serialize(formatUpperCase, base)
            } yield str
          )
        )
      case _ =>
        err(s"Not implemented conversion to $format. Schema: $schema")
    }
  }

  def fromFile(
      fileName: String,
      format: String = "ShExC",
      base: Option[IRI] = None,
      maybeRDFBuilder: Option[RDFBuilder] = None
  ): IO[Schema] = for {
    cs <- FileUtils.getContents(Paths.get(fileName))
    schema <- fromString(cs, format, base, maybeRDFBuilder)
  } yield schema

  /** @param reader input reader
    * @param format format of reader. Default value = ShExC, other values = ShExJ, ShExR
    * @param base optional IRI that acts as base, default value = None
    * @param maybeRDFBuilder RDFBuilder
    */
  def fromString(
      str: String,
      format: String = "ShExC",
      base: Option[IRI] = None,
      maybeRDFBuilder: Option[RDFBuilder] = None
  ): IO[Schema] = {
    val is = new ByteArrayInputStream(str.getBytes())
    for {
      schema <- fromInputStream(is, format, base, maybeRDFBuilder)
    } yield schema
  }

  import java.net.URI

  private def derefUri(uri: URI, verbose: VerboseLevel): IO[String] =
    Try {
      val urlCon = uri.toURL.openConnection()
      urlCon.setConnectTimeout(10000)
      urlCon.setReadTimeout(10000)
      val is = urlCon.getInputStream()
      Source.fromInputStream(is).mkString
    }.fold(
      e => {
        verbose.info(s"Error trying to access $uri: ${e.getMessage()}\n${e.getClass()}\n")
        IO.raiseError(e)
      },
      IO(_)
    )

  private def getContents(is: InputStream): IO[String] =
    IO(scala.io.Source.fromInputStream(is).mkString)

}
