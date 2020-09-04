package es.weso.shex

import java.nio.file.{Files, Paths}
import cats.syntax.all._
import es.weso.depgraphs.DepGraph
import es.weso.rdf.{PrefixMap, RDFBuilder, RDFReader}
import es.weso.rdf.nodes._
import es.weso.shex.shexR.{RDF2ShEx, ShEx2RDF}
import scala.io.Source
import scala.util._
import cats.effect.IO

case class Schema(id: IRI,
                  prefixes: Option[PrefixMap],
                  base: Option[IRI],
                  startActs: Option[List[SemAct]],
                  start: Option[ShapeExpr],
                  shapes: Option[List[ShapeExpr]],
                  optTripleExprMap: Option[Map[ShapeLabel,TripleExpr]],
                  imports: List[IRI]
                 ) extends AbstractSchema {

  def addShape(se: ShapeExpr): Schema = this.copy(shapes = addToOptionList(se,shapes))

  def getTripleExpr(lbl: ShapeLabel): Either[String,TripleExpr] = 
    optTripleExprMap match {
      case None => Left(s"Cannot find $lbl with an empty tripleExpr map")
      case Some(m) => m.get(lbl).toRight(s"Not found $lbl in map $m")
    }

  def shapeLabel2Iri(l: ShapeLabel): Either[String, IRI] = l match {
    case IRILabel(iri) => Right(iri)
    case _ => Left(s"Label $l can't be converted to IRI")
  }

  def addId(i: IRI): Schema = this.copy(id = i)

  def getShape(label: ShapeLabel): Either[String,ShapeExpr] = for {
    se <- shapesMap.get(label) match {
      case None => s"Not found $label in schema. Available labels: ${shapesMap.keySet.mkString}".asLeft[ShapeExpr]
      case Some(se) => se.asRight[String]
    }
  } yield se

  def err[A](msg: String): IO[A] = IO.raiseError(new RuntimeException(msg))
  def ok[A](x:A): IO[A] = IO.pure(x)
  
  lazy val localShapes: List[ShapeExpr] = shapes.getOrElse(List())

  lazy val shapeList: List[ShapeExpr] = shapes.getOrElse(List())
   /* eitherResolvedShapesMap.fold(_ => localShapes,
     sm => sm.values.toList) */

  override def labels: List[ShapeLabel] = localShapes.map(_.id).flatten

  def addTripleExprMap(te: Map[ShapeLabel,TripleExpr]): Schema =
    this.copy(optTripleExprMap = Some(te))

  def oddNegCycles: Either[String,Set[Set[(ShapeLabel,ShapeLabel)]]] =
    Dependencies.oddNegCycles(this)

  def negCycles: Either[String, Set[Set[(ShapeLabel,ShapeLabel)]]] =
    Dependencies.negCycles(this)

  def depGraph: Either[String, DepGraph[ShapeLabel]] =
    Dependencies.depGraph(this)

  def showCycles(str: Either[String,Set[Set[(ShapeLabel,ShapeLabel)]]]): String = str match {
    case Left(e) => e
    case Right(ss) => ss.map(s => s.map(_.toString).mkString(",")).mkString("\n")
  }
/*  lazy val listNegCycles: List[String] = negCycles.fold(
    e => List(e),
    ns => if (ns.isEmpty) List()
    else
      List(s"Negative cycles found: [${ns.map(s => s.map(_.toString).mkString(",")).mkString(",")}]")
  ) */


 private def checkShapeLabel(lbl: ShapeLabel): Either[String, Unit] = for {
   se <- getShape(lbl)
   //_ <- { println(s"Label ${lbl.toString}. ShapeExpr: ${se.toString}"); Right(())}
   refs <- se.getShapeRefs(this)
   //_ <- { println(s"Refs ${refs.toString}"); Right(())}
  } yield {
    // println(s"Label: $lbl, refs: ${se.getShapeRefs(this).mkString(",")}")
    // println(s"References: ${refs.mkString(",")}")
    ()
  }

  private lazy val checkBadShapeLabels: Either[String,Unit] = for {
    //_ <- { println(s"shapesMap: $shapesMap"); Right(())}
    _ <- shapesMap.keySet.toList.map(lbl => checkShapeLabel(lbl)).sequence
  } yield (()) 


  private lazy val checkOddNegCycles: Either[String, Unit] = {
    // println(s"OddNegCycles: $oddNegCycles")
    oddNegCycles match {
      case Left(e) => Left(e)
      case Right(cs) => if (cs.isEmpty) Right(())
      else
        Left(s"Negative cycles: ${showCycles(oddNegCycles)}")
    }
  }

  lazy val wellFormed: Either[String,Unit] = for {
    _ <- checkOddNegCycles
    //_ <- { println(s"Passed checkOddNegCycles..."); Right(())}
    _ <- checkBadShapeLabels
    //_ <- { println(s"Passed checkBadShapeLabels..."); Right(())}
  } yield (())

  def relativize(maybeBase: Option[IRI]): Schema = maybeBase match {
    case None => this
    case Some(baseIri) => Schema(
      id.relativizeIRI(baseIri),
      prefixes,
      base.map(_.relativizeIRI(baseIri)),
      startActs,
      start.map(_.relativize(baseIri)),
      shapes.map(_.map(_.relativize(baseIri))),
      optTripleExprMap,
      imports
    )
  }

  private def addToOptionList[A](x: A, maybeLs: Option[List[A]]): Option[List[A]] = maybeLs match {
    case None => Some(List(x))
    case Some(xs) => Some(x :: xs)
  }

}


object Schema {

  def rdfDataFormats(rdfReader: RDFReader) = rdfReader.availableParseFormats.map(_.toUpperCase)

  def empty: Schema =
    Schema(IRI(""),None, None, None, None, None, None, List())

  def fromIRI(i: IRI, base: Option[IRI]): IO[Schema] = {
    val uri = i.uri
    if (uri.getScheme == "file") {
        if (Files.exists(Paths.get(i.uri))) {
            val str = Source.fromURI(uri).mkString
            fromString(str, "ShExC", Some(i)).map(schema => schema.addId(i))
    } else {
          val iriShEx = i + ".shex"
          if (Files.exists(Paths.get((iriShEx).uri))) {
            val str = Source.fromURI(iriShEx.uri).mkString
            fromString(str, "ShExC", Some(i)).map(schema => schema.addId(i))
          } else {
            val iriJson = i + ".json"
            if (Files.exists(Paths.get((iriJson).uri))) {
            val str = Source.fromURI(iriJson.uri).mkString
            fromString(str, "JSON", Some(i)).map(schema => schema.addId(i))
           }
           else {
            //println(s"File $i does not exist")
            err(s"File $i does not exist")
           }
        }}}
      else
       for {
         schema <- getSchemaWithExts(i, List(("","ShExC"),("shex","ShExC"), ("json", "JSON")),base)
       } yield schema.addId(i)
  }

  private def getSchemaWithExts(iri: IRI, exts: List[(String,String)], base: Option[IRI] ): IO[Schema] = 
  exts match {
    case (e :: es) => getSchemaExt(iri,e,base) orElse getSchemaWithExts(iri,es,base)
    case Nil => err(s"Can not obtain schema from iri: $iri, Exts: ${exts} ")
  }

  private def getSchemaExt(iri: IRI, pair: (String,String), base: Option[IRI]): IO[Schema] = {
   println(s"getSchemaExt: ${iri} ${pair}") 
   val (ext,format) = pair
   val uri = if (ext == "") iri.uri
   else (iri + "." + ext).uri
   for {
    _ <- { println(s"Trying to deref $uri"); IO.pure(()); }
    str <- derefUri(uri)
    _ <- { println(s"Str obtained...${str.linesIterator.take(2).mkString("\n")}\n..."); IO.pure(()); }
    schema <- Schema.fromString(str,format,base,None)
    _ <- { println(s"Obtained schema at $uri"); IO.pure(()); }
   } yield schema
  }

  import java.net.URI

  def derefUri(uri: URI): IO[String] = {
    Try {
        val urlCon = uri.toURL.openConnection()
        urlCon.setConnectTimeout(10000)
        urlCon.setReadTimeout(10000)
        val is = urlCon.getInputStream()
        Source.fromInputStream(is).mkString
    }.fold(e => {
      println(s"Error trying to access $uri: ${e.getMessage()}\n${e.getClass()}\n")
      IO.raiseError(e)
    }, IO(_))
  }


  /**
  * Reads a Schema from a char sequence
    * @param cs char sequence
    * @param format syntax format
    * @param base base URL
    * @param maybeRDFReader RDFReader value from which to obtain RDF data formats (in case of RDF format)
    * @return either a Schema or a String message error
    */
  def fromString(cs: CharSequence,
                 format: String = "ShExC",
                 base: Option[IRI] = None,
                 maybeRDFReader: Option[RDFReader] = None
                ): IO[Schema] = {
    val formatUpperCase = format.toUpperCase
    formatUpperCase match {
      case "SHEXC" => {
        import compact.Parser.parseSchema
        parseSchema(cs.toString, base).fold(e => err(e), ok)
      }
      case "SHEXJ" => {
        import io.circe.parser._
        import es.weso.shex.implicits.decoderShEx._
        decode[Schema](cs.toString).leftMap(_.getMessage).fold(e => err(e), ok)
      }
      case _ => maybeRDFReader match {
        case None => err(s"Not implemented ShEx parser for format $format and no rdfReader provided")
        case Some(rdfReader) =>
         if (rdfDataFormats(rdfReader).contains(formatUpperCase)) for {
          rdf    <- rdfReader.fromString(cs, formatUpperCase, base)
          eitherSchema <- RDF2ShEx.rdf2Schema(rdf).value
          schema <- eitherSchema.fold(e => err(e), ok)
         } yield schema
         else err(s"Not implemented ShEx parser for format $format")
       }
    }
  }

  def err[A](msg:String): IO[A] = IO.raiseError(new RuntimeException(msg))
  def ok[A](v:A): IO[A] = IO.pure(v)

  def serialize(schema: Schema,
                format: String,
                base: Option[IRI],
                rdfBuilder: RDFBuilder): IO[String] = {
    val formatUpperCase = format.toUpperCase
    val relativeSchema = schema.relativize(base)
    formatUpperCase match {
      case "SHEXC" => {
        import compact.CompactShow._
        IO.pure(showSchema(relativeSchema))
      }
      case "SHEXJ" => {
        import io.circe.syntax._
        import es.weso.shex.implicits.encoderShEx._
        IO.pure(relativeSchema.asJson.spaces2)
      }
      case _ if (rdfDataFormats(rdfBuilder).contains(formatUpperCase)) => for {
        empty <- rdfBuilder.empty
        rdf <- ShEx2RDF(relativeSchema, None, empty)
        str <- rdf.serialize(formatUpperCase, base)
      } yield str
      case _ =>
        err(s"Not implemented conversion to $format. Schema: $schema")
    }
  }

//  def resolveSchema: IO[ResolvedSchema] = ???

}
