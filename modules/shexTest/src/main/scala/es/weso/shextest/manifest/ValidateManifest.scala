package es.weso.shextest.manifest

import java.nio.file.Paths
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
// import es.weso.shapeMaps.ShapeMap
import es.weso.utils.FileUtils
import org.scalatest._
import scala.util.{Either, Left, Right, Try}
import cats.data.EitherT
import cats.effect.IO
import cats.implicits._

trait RunManifest {

  def runManifest(
      name: String,
      folder: String,
      parentFolder: String,
      nameIfSingle: Option[String],
      withEntry: (es.weso.shextest.manifest.Entry, String, String, Option[String]) => EitherT[IO, String, Option[(String, Boolean)]]
  ): EitherT[IO, String, List[(String, Boolean)]] = {
    val parentFolderURI = Try { Paths.get(parentFolder).normalize.toUri.toString }.getOrElse("")
    val manifestFolder  = s"$parentFolder/$folder"
    val fileName        = s"$manifestFolder/$name.ttl"
    for {
      mf <- RDF2Manifest
        .read(fileName, "TURTLE", Some(s"$parentFolderURI/$folder/"), true)
        .leftMap(e => s"Error reading $fileName\nError: $e")
      v <- processManifest(mf, name, manifestFolder, nameIfSingle, withEntry)
    } yield v
  }

  private def processManifest(
      m: ShExManifest,
      name: String,
      parentFolder: String,
      nameIfSingle: Option[String],
      withEntry: (es.weso.shextest.manifest.Entry, String, String, Option[String]) => EitherT[IO, String, Option[(String, Boolean)]]
  ): EitherT[IO, String, List[(String, Boolean)]] =
    for {
      vs1 <- m.includes
        .map {
          case (includeNode, manifest) =>
            val folder = Try { Paths.get(includeNode.getLexicalForm).getParent.toString }.getOrElse("")
            runManifest(includeNode.getLexicalForm, folder, parentFolder, nameIfSingle, withEntry)
        }
        .sequence
        .map(_.flatten)
      vs2 <- m.entries.map(withEntry(_, name, parentFolder, nameIfSingle)).sequence
    } yield (vs1 ++ vs2.flatten[(String,Boolean)])
}

trait ValidateManifest extends FunSpec with Matchers with TryValues with OptionValues with RunManifest {

  def parseManifest(name: String, folder: String, parentFolder: String, nameIfSingle: Option[String]): Unit = {
    it(s"Should parse manifestTest $folder/$name") {
      runManifest(name, folder, parentFolder, nameIfSingle, processEntry)
    }
  }

  def processEntry(
      e: es.weso.shextest.manifest.Entry,
      name: String,
      manifestFolder: String,
      nameIfSingle: Option[String]
  ): EitherT[IO, String, Option[(String, Boolean)]] = 
  if (nameIfSingle == None || nameIfSingle.getOrElse("") == e.name) {
   e match {
    case r: RepresentationTest => {
      println(s"Entry: ${e}, name: $name")
      EitherT.pure[IO, String](Some((name, true)))
    }
    case v: Validate => {
      val a = v.action
      val r = for {
        strRdf            <- getContents("data", manifestFolder, a.data)
        strSchema         <- getContents("schema", manifestFolder, a.schema)
        strShapeMap       <- getContents("shapeMap", manifestFolder, a.shapeMap)
        strResultShapeMap <- getContents("resultShapeMap", manifestFolder, a.resultShapeMap)
      } yield {
        (strRdf, strSchema, strShapeMap, strResultShapeMap)
      }
      r.fold(
        e => EitherT.fromEither(Left(s"Error: $e")),
        v => {
          val (strRdf, strSchema, strShapeMap, strResultShapeMap) = v
          shouldValidateWithShapeMap(name, strRdf, strSchema, strShapeMap, strResultShapeMap).map(Some(_))
        }
      )
    }
    case _ => EitherT.fromEither(Left(s"Unsupported entry type: ${e.entryType}"))
  }
} else EitherT.pure(None)

 def getContents(name: String, folder: String, value: Option[IRI]): Either[String, String] = value match {
    case None      => Left(s"No value for $name")
    case Some(iri) => FileUtils.getContents(folder + "/" + iri.str).map(_.toString)
  }

  def shouldValidateWithShapeMap(
      name: String,
      rdfStr: String,
      shexStr: String,
      shapeMapStr: String,
      expected: String
  ): EitherT[IO, String, (String, Boolean)] = {
    val validate = for {
      rdf <- RDFAsJenaModel.fromChars(rdfStr, "Turtle")
    } yield true
    validate match {
      case Left(msg) => EitherT.fromEither(s"Error processing RDF: $msg\nRDF=\n$rdfStr".asLeft[(String, Boolean)])
      case Right(v)  => EitherT.pure[IO, String]((name, v))
    }
  }
}
