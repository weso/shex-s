package es.weso.shapemaps

import es.weso.rdf.nodes._
import cats._
import cats.implicits._
import es.weso.rdf.{PrefixMap, RDFReader}
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import ShapeMap._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.path._
import scala.io.Source
import scala.util.Try
import cats.effect.IO
import java.nio.file.Path
import es.weso.utils.FileUtils
import scala.util.control.NoStackTrace
import java.io.InputStreamReader
import java.io.InputStream
import cats.data.NonEmptyList

abstract class ShapeMap {
  val associations: List[Association]
  def isEmpty = associations.isEmpty
  val nodesPrefixMap: PrefixMap
  val shapesPrefixMap: PrefixMap

  def add(node: RDFNode, label: ShapeMapLabel): Either[String,ShapeMap] =
    addAssociation(Association(RDFNodeSelector(node),label))

  def addAssociation(a: Association): Either[String, ShapeMap]

  def toJson: Json = {
    this.asJson
  }

  def serialize( format: String, 
                 base: Option[IRI] = None,
                ): Either[String,String] = {
    ShapeMapFormat.fromString(format).map(_ match {
      case Compact => this.relativize(base).showShapeMap(withDetails = false)
      case JsonShapeMapFormat => this.toJson.spaces2
      case CompactDetails => this.relativize(base).showShapeMap(withDetails = true)
    })
  }

  def relativize(base: Option[IRI]): ShapeMap

  private def showPattern(p: Pattern, pm: PrefixMap): String = p match {
    case NodePattern(node) => pm.qualify(node)
    case WildCard => "_"
    case Focus => "FOCUS"
  }

  private def showPath(path: SHACLPath, pm: PrefixMap): String = path match {
    case PredicatePath(`rdf:type`) => "a"
    case PredicatePath(iri) => pm.qualify(iri)
    case InversePath(path) => s"^${showPath(path, pm)}"
    case SequencePath(paths) => paths.map(showPath(_, pm)).mkString("/")
    case AlternativePath(paths) => paths.map(showPath(_,pm)).mkString("|")
    case OneOrMorePath(path) => s"${showPath(path,pm)}+"
    case ZeroOrMorePath(path) => s"${showPath(path,pm)}*"
    case ZeroOrOnePath(path) => s"${showPath(path,pm)}?"
  }

  private def showNodeSelector(n: NodeSelector, pm: PrefixMap): String = {
    n match {
            case RDFNodeSelector(node) => pm.qualify(node)
            case TriplePattern(sub, path, obj) => s"{${showPattern(sub,pm)} ${showPath(path,pm)} ${showPattern(obj,pm)}}"
            case SparqlSelector(query) => s"""SPARQL `$query`"""
    }
  }

  private def showShapeMapLabel(label: ShapeMapLabel, pm: PrefixMap): String = label match {
          case IRILabel(iri) => pm.qualify(iri)
          case BNodeLabel(bn) => "_:" ++ bn.getLexicalForm
          case Start => "Start"
  }

  def showShapeMap(withDetails: Boolean): String = { 
   val sep = if (withDetails) ",\n" else ", "
   if (associations.isEmpty) 
    s"# Empty shape map"
   else 
    associations.map(a => showAssociation(a, nodesPrefixMap, shapesPrefixMap, withDetails)).mkString(sep)
  }


  override def toString: String = showShapeMap(withDetails = false)


  private def showAssociation(
    a: Association, 
    nodesPrefixMap: PrefixMap, 
    shapesPrefixMap: PrefixMap, 
    withDetails: Boolean
    ): String = {
    s"${showNodeSelector(a.node, nodesPrefixMap)}@${if (a.info.status==NonConformant) "!" else ""}${showShapeMapLabel(a.shape,shapesPrefixMap)}${if (withDetails) showDetails(a.info) else ""}"
  }

  private def showDetails(info: Info): String = {
    val tab = " " * 3
    val str = info.reason.getOrElse("").linesIterator.map(line => s"${tab}# ${line}").mkString("\n")
    s"\n$str"
  }


}

object ShapeMap {

  def availableFormats: List[String] = ShapeMapFormat.availableFormatNames
  def empty: ShapeMap = FixedShapeMap.empty

  def fromInputStream(
    is: InputStream, 
    format: String, 
    base: Option[IRI], 
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap
    ): Either[NonEmptyList[String],ShapeMap] = {
      val reader = new InputStreamReader(is)
      Parser.parseReader(reader, base, nodesPrefixMap, shapesPrefixMap)
    }

  def fromURI(uri: String,
              format: String,
              base: Option[IRI],
              nodesPrefixMap: PrefixMap,
              shapesPrefixMap: PrefixMap): IO[Either[NonEmptyList[String], ShapeMap]] = {
   val t = Try {
      val contents = Source.fromURL(uri).mkString
      val either: Either[NonEmptyList[String], ShapeMap] = {
        fromString(contents, format, base, nodesPrefixMap, shapesPrefixMap)
      }
      either
    }
   t.fold(
     e => IO.raiseError(new RuntimeException(s"Exception obtaining URI contents. URI = ${uri}. Error: ${e.getLocalizedMessage}")),
     e => IO.pure(e)
   )
  }

  case class ShapeMapFromPathException(errors: NonEmptyList[String], path: Path, format: String, base: Option[IRI], nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap) 
    extends RuntimeException(s"""|Error obtaining shapeMap from path
                                 |Errors: ${errors.toList.mkString("\n")}
                                 |Absolute path: ${path.toFile().getAbsolutePath()}
                                 |Format: $format
                                 |Base: ${base.map(_.getLexicalForm).getOrElse("")}
                                 |NodesPrefixMap: ${nodesPrefixMap.toString()}
                                 |ShapesPrefixMap: ${shapesPrefixMap.toString()}
                                 |""".stripMargin) 
    with NoStackTrace

  def fromPath(path: Path, 
      format: String, 
      base: Option[IRI] = None, 
      nodesPrefixMap: PrefixMap = PrefixMap.empty, 
      shapesPrefixMap: PrefixMap = PrefixMap.empty): IO[ShapeMap] = for {
    str <- FileUtils.getContents(path)
    sm <- fromString(str, format, base, nodesPrefixMap, shapesPrefixMap).fold(
      nes => IO.raiseError(ShapeMapFromPathException(nes,path,format,base,nodesPrefixMap,shapesPrefixMap)),
      sm => IO.pure(sm)
    )
  } yield sm

  def fromString(str: String,
                 format: String,
                 base: Option[IRI] = None,
                 nodesPrefixMap: PrefixMap = PrefixMap.empty,
                 shapesPrefixMap: PrefixMap = PrefixMap.empty
                ): Either[NonEmptyList[String],ShapeMap] =
    format.toUpperCase match {
     case "JSON" => fromJson(str)
     case "COMPACT" => {
       fromCompact(str,base,nodesPrefixMap,shapesPrefixMap)
     }
     case _ => Left(NonEmptyList.one(s"Unknown format for shapeMap"))
   }

  def fromCompact(
    str: String,
    base: Option[IRI] = None,
    nodesPrefixMap: PrefixMap = PrefixMap.empty,
    shapesPrefixMap: PrefixMap = PrefixMap.empty): Either[NonEmptyList[String], ShapeMap] = {
    if (str.isEmpty) Right(ShapeMap.empty)
    else Parser.parse(str, base, nodesPrefixMap, shapesPrefixMap)
  }

  def fromJson(jsonStr: String): Either[NonEmptyList[String], ShapeMap] = {
    decode[ShapeMap](jsonStr).leftMap(s => NonEmptyList.one(s.getMessage))
  }

  def parseResultMap(
    str: String,
    base: Option[IRI],
    rdf: RDFReader,
    shapesPrefixMap: PrefixMap = PrefixMap.empty): IO[ResultShapeMap] =
     for {
      rdfPrefixMap <- rdf.getPrefixMap
      queryMap <- IO {
       Parser.parse(str, base, rdfPrefixMap, shapesPrefixMap).fold(e =>
        throw new RuntimeException(s"Error parsing as ShapeMap str:$str\nError: $e"),
        identity
       )
     }
    fixMap <- fixShapeMap(queryMap, rdf, rdfPrefixMap, shapesPrefixMap)
  } yield {
    ResultShapeMap(fixMap.shapeMap, rdfPrefixMap, shapesPrefixMap)
  }
  /**
   * Resolve triple patterns according to an RDF
   */
  def fixShapeMap(
    shapeMap: ShapeMap,
    rdf: RDFReader,
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap): IO[FixedShapeMap] = {

    val empty: IO[FixedShapeMap] = IO.pure(
      FixedShapeMap.empty.
        addNodesPrefixMap(nodesPrefixMap).
        addShapesPrefixMap(shapesPrefixMap)
    )

    def addNode(a: Association)(
      node: RDFNode,
      current: IO[FixedShapeMap]
    ): IO[FixedShapeMap] = for {
      fixed <- current
      newShapeMap <- IO {
        fixed.addAssociation(Association(
          RDFNodeSelector(node), a.shape, a.info)
        ).fold(e => throw new RuntimeException(s"Error adding association: $a to $fixed"), identity)
      }
    } yield newShapeMap

    def combine(a: Association,
                current: IO[FixedShapeMap]
               ): IO[FixedShapeMap] = {
      for {
        nodes <- a.node.select(rdf).compile.toList
        r <- nodes.foldRight(current)(addNode(a))
      } yield r
    }
    shapeMap.associations.foldRight(empty)(combine)
  }

  implicit val encodeShapeMap: Encoder[ShapeMap] = new Encoder[ShapeMap] {
    final def apply(a: ShapeMap): Json = a.associations.asJson
  }

  implicit val showShapeMap: Show[ShapeMap] = new Show[ShapeMap] {
    final def show(s: ShapeMap): String = s.toString
  }


  implicit val decodeShapeMap: Decoder[ShapeMap] = Decoder.instance { c =>
    for {
      associations <- c.as[List[Association]]
    } yield QueryShapeMap(associations, PrefixMap.empty, PrefixMap.empty)
  }

  def formats: List[String] = List("COMPACT", "JSON")

  def defaultFormat = formats.head
}
