package es.weso.shextest.manifest

import java.net.URI
import java.nio.file.Paths
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.utils.UriUtils._
import es.weso.shapeMaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMap, _}
// import es.weso.shapeMaps.ShapeMap
import es.weso.utils.FileUtils
import org.scalatest._
import scala.util.{Either, Left, Right, Try}
import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import es.weso.shex._
import es.weso.shex.validator.{ExternalIRIResolver, Validator}
import es.weso.shex.compact.CompareSchemas
import es.weso.shextest.manifest.Utils._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import scala.io._
import io.circe.parser._
import io.circe.syntax._

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
      runManifest(name, folder, parentFolder, nameIfSingle, processEntryValidating)
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
    case v: ValidationTest => {
      val base = Paths.get(".").toUri
      v.action match {
        case focusAction: FocusAction => validateFocusAction(focusAction,base,v,true)
        case mr: MapResultAction => validateMapResult(mr,base,v)
        case ma: ManifestAction => fail(s"Not implemented validate ManifestAction yet")
      }
    }
    case _ => EitherT.fromEither(Left(s"Unsupported entry type: ${e.entryType}"))
  }
} else EitherT.pure(None)

def processEntryValidating(
  e: es.weso.shextest.manifest.Entry,
  name: String,
  manifestFolder: String,
  nameIfSingle: Option[String]
): EitherT[IO, String, Option[(String, Boolean)]] = 
if (nameIfSingle == None || nameIfSingle.getOrElse("") == e.name) {
e match {
 case r: RepresentationTest => {
  val manifestFolderURI = Paths.get(manifestFolder).normalize.toUri
  val resolvedJson = mkLocal(r.json,schemasBase,manifestFolderURI)// IRI(shexFolderURI).resolve(r.json).uri
  val resolvedShEx = mkLocal(r.shex,schemasBase,manifestFolderURI)// IRI(shexFolderURI).resolve(r.shex).uri
// info(s"Entry: $r with json: ${resolvedJsonIri}")
val jsonStr   = Source.fromURI(resolvedJson)("UTF-8").mkString
val schemaStr = Source.fromURI(resolvedShEx)("UTF-8").mkString
Schema.fromString(schemaStr, "SHEXC", None) match {
  case Right(schema) => {
    decode[Schema](jsonStr) match {
      case Left(err) => fail(s"Error parsing Json ${r.json}: $err")
      case Right(expectedSchema) =>
        if (CompareSchemas.compareSchemas(schema, expectedSchema)) {
          parse(jsonStr) match {
            case Left(err) => fail(s"Schemas are equal but error parsing Json $jsonStr")
            case Right(json) => {
              if (json.equals(schema.asJson)) {
                ok(Some((name,true)))
              } else {
                fail(
                  s"Json's are different\nSchema:${schema}\nJson generated: ${schema.asJson.spaces2}\nExpected: ${json.spaces2}")
              }
            }
          }
        } else {
          fail(s"Schemas are different. Parsed:\n${schema}\n-----Expected:\n${expectedSchema}")
        }
    }
  }
  case Left(e) => fail(s"Error parsing Schema: ${r.shex}: $e")
}
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
    e => fail(s"Error: $e"),
    v => {
      val (strRdf, strSchema, strShapeMap, strResultShapeMap) = v
      shouldValidateWithShapeMap(name, strRdf, strSchema, strShapeMap, strResultShapeMap).map(Some(_))
    }
  )
}
case _ => fail(s"Unsupported entry type: ${e.entryType}")
}
} else EitherT.pure(None)

 def fail[A](msg: String): EitherT[IO,String,A] = EitherT.fromEither(Left(msg))
 def ok[A](x:A): EitherT[IO,String,A] = EitherT.pure(x)

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

  /*
  def validateFocusAction(fa: FocusAction,
                          base: URI,
                          v: ValidOrFailureTest,
                          shouldValidate: Boolean
                         ): Either[String, String] = {
    val focus = fa.focus
    val schemaUri = mkLocal(fa.schema,schemasBase,shexFolderURI)
    val dataUri = mkLocal(fa.data,schemasBase,shexFolderURI)
    for {
      schemaStr <- derefUri(schemaUri)
      dataStr <- derefUri(dataUri)
      schema <- Schema.fromString(schemaStr, "SHEXC", Some(fa.schema))
      data   <- RDFAsJenaModel.fromChars(dataStr, "TURTLE", Some(fa.data))
      lbl = fa.shape match {
        case None           => StartMap: ShapeMapLabel
        case Some(i: IRI)   => IRIMapLabel(i)
        case Some(b: BNode) => BNodeMapLabel(b)
        case Some(other) => {
          IRIMapLabel(IRI(s"UnknownLabel"))
        }
      }
      ok <- if (v.traits contains sht_Greedy) {
        Right(s"Greedy")
      } else {
        val shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), data.getPrefixMap, schema.prefixMap)
        for {
          resultShapeMap <- Validator(schema, ExternalIRIResolver(fa.shapeExterns))
            .validateShapeMap(data, shapeMap).toEitherS
          ok <- if (resultShapeMap.getConformantShapes(focus) contains lbl)
            if (shouldValidate) Right(s"Focus $focus conforms to $lbl as expected")
            else Left(s"Focus $focus conforms to $lbl but should not" ++
                      s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
                      s"${resultShapeMap.getInfo(focus, lbl)}\n" ++
                      s"Schema: ${schema}\n" ++
                      s"Data: ${data}")
          else {
            if (!shouldValidate) Right(s"Focus $focus does not conforms to $lbl as expected")
            else Left(s"Focus $focus does not conform to $lbl but should" ++
              s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
              s"${resultShapeMap.getInfo(focus, lbl)}\n" ++
              s"Schema: ${schema}\n" ++
              s"Data: ${data}")
          }
        } yield ok
      }
    } yield ok
  }

  def validateMapResult(mr: MapResultAction,
                        base: URI,
                        v: ValidOrFailureTest
                       ): Either[String,String] = {
    v.maybeResult match {
      case None => Left(s"No result specified")
      case Some(resultIRI) => {
        val schemaUri         = mkLocal(mr.schema, validationBase, shexFolderURI)
        val shapeMapUri       = mkLocal(mr.shapeMap, validationBase, shexFolderURI)
        val resultMapUri      = mkLocal(resultIRI, validationBase, shexFolderURI)
        val r: Either[String, String] = for {
          schemaStr      <- derefUri(schemaUri)
          resultMapStr  <- derefUri(resultMapUri)
          smapStr       <- derefUri(shapeMapUri)
          sm            <- ShapeMap.fromJson(smapStr)
          schema        <- Schema.fromString(schemaStr, "SHEXC", None)
          fixedShapeMap <- ShapeMap.fixShapeMap(sm, RDFAsJenaModel.empty, PrefixMap.empty, PrefixMap.empty)
          dataUri = mkLocal(mr.data,schemasBase,shexFolderURI)
          strData        <- derefUri(dataUri)
          data           <- RDFAsJenaModel.fromChars(strData, "TURTLE", None)
          resultShapeMap <- Validator(schema).validateShapeMap(data, fixedShapeMap).toEitherS
          jsonResult     <- JsonResult.fromJsonString(resultMapStr)
          result <- if (jsonResult.compare(resultShapeMap)) Right(s"Json results match resultShapeMap")
          else
            Left(
              s"Json results are different. Expected: ${jsonResult.asJson.spaces2}\nObtained: ${resultShapeMap.toString}")
        } yield result
        r
      }
    }
  } */

  
}
