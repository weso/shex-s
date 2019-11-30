package es.weso.shextest.manifest

import java.net.URI
import java.nio.file.Paths
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
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
import es.weso.rdf._
import es.weso.rdf.nodes._
import ManifestPrefixes._

trait RunManifest {

  case class Result(name: String, isOk: Boolean, reason: String)
  case class EntryParam(entry: es.weso.shextest.manifest.Entry, name: String, parentFolder: String, nameIfSingle: Option[String])
  type EntryProcess = EntryParam => EitherT[IO, String, Option[Result]]

  def runManifest(
      name: String,
      folder: String,
      parentFolder: String,
      nameIfSingle: Option[String],
      withEntry: EntryProcess
  ): EitherT[IO, String, List[Result]] = {
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
      withEntry: EntryProcess
  ): EitherT[IO, String, List[Result]] =
    for {
      vs1 <- m.includes
        .map {
          case (includeNode, manifest) =>
            val folder = Try { Paths.get(includeNode.getLexicalForm).getParent.toString }.getOrElse("")
            runManifest(includeNode.getLexicalForm, folder, parentFolder, nameIfSingle, withEntry)
        }
        .sequence
        .map(_.flatten)
      vs2 <- m.entries.map(e => withEntry(EntryParam(e, name, parentFolder, nameIfSingle))).sequence
    } yield (vs1 ++ vs2.flatten[Result])
}

trait ValidateManifest extends FunSpec with Matchers with TryValues with OptionValues with RunManifest {

  def showFailed(vs: List[Result], withReason: Boolean): String = {
    val failed = vs.filter(_.isOk == false )
    if (failed.length == 0) 
      "No failed tests"
    else 
      failed.map(f => s"${f.name}${if (withReason) s": ${f.reason}" else ""}").mkString("\n") ++
      s"\nNumber of failed tests: ${failed.length}\n" 
  }

  def parseManifestValidating(
      name: String,
      folder: String,
      parentFolder: String,
      nameIfSingle: Option[String]
  ): Unit = {
    it(s"Should parse manifestTest $folder/$name") {
      val r = runManifest(name, folder, parentFolder, nameIfSingle, processEntryValidating)
      r.value.unsafeRunSync.fold(e => {
        val currentFolder = new java.io.File(".").getCanonicalPath
        fail(s"Error: $e\nCurrent folder: $currentFolder")
    }, vs => {
        info(s"Number of tests run: ${vs.length}\n${showFailed(vs,true)}")
      })
    }
  }

  def parseManifest(
      name: String,
      folder: String,
      parentFolder: String,
      nameIfSingle: Option[String],
      processEntry: EntryParam => EitherT[IO, String, Option[Result]]
  ): Unit = {
    it(s"Should parse manifestTest $folder/$name") {
      val r = runManifest(name, folder, parentFolder, nameIfSingle, processEntry)
      r.value.unsafeRunSync.fold(e => fail(s"Error: $e"), vs => {
        info(s"Number of tests run: ${vs.length}")
        info(s"Failed tests: ${showFailed(vs,false)}")
      })
    }
  }

/*  def processEntry: EntryProcess = ep => 
    if (ep.nameIfSingle == None || ep.nameIfSingle.getOrElse("") == ep.entry.name) {
      val folderURI = Paths.get(ep.parantFolder).normalize.toUri
      e match {
        case r: RepresentationTest => {
          println(s"Entry: ${ep.entry}, name: ${ep.name}")
          EitherT.pure[IO, String](Some(Result(ep.name, true, "passed representation test")))
        }
        case v: Validate => {
          val a = v.action
          val r = for {
            strRdf            <- getContents("data", ep.parantFolder, a.data)
            strSchema         <- getContents("schema", ep.parentFolder, a.schema)
            strShapeMap       <- getContents("shapeMap", ep.parentFolder, a.shapeMap)
            strResultShapeMap <- getContents("resultShapeMap", ep.parentFolder, a.resultShapeMap)
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
            case focusAction: FocusAction => validateFocusAction(focusAction, base, v, true, name, folderURI)
            case mr: MapResultAction      => validateMapResult(mr, base, v, name, folderURI)
            case ma: ManifestAction       => fail(s"Not implemented validate ManifestAction yet")
          }
        }
        case _ => EitherT.fromEither(Left(s"Unsupported entry type: ${e.entryType}"))
      }
    } else EitherT.pure(None)
*/
  def processEntryValidating: EntryProcess = ep => {
    if (ep.nameIfSingle == None || ep.nameIfSingle.getOrElse("") == ep.entry.name) {
      val folderURI = Paths.get(ep.parentFolder).normalize.toUri
      val base = Paths.get(".").toUri
       
      ep.entry match {

        case v: ValidationTest => {
          v.action match {
            case focusAction: FocusAction => validateFocusAction(focusAction, base, v, true, v.name, folderURI)
            case mr: MapResultAction      => validateMapResult(mr, base, v, v.name, folderURI)
            case ma: ManifestAction       => result(v.name, false, s"Not implemented validate ManifestAction yet")
          }
        }
       
        case v: ValidationFailure => {
          v.action match {
            case focusAction: FocusAction => validateFocusAction(focusAction, base, v, false, v.name, folderURI)
            case mr: MapResultAction      => result(v.name, false, s"Not implemented validationFailure MapResultAction yet")
            case ma: ManifestAction       => result(v.name, false, s"Not implemented validationFailure ManifestAction yet")
          }
        }
       
        case r: RepresentationTest => {
          val manifestFolderURI = Paths.get(ep.parentFolder).normalize.toUri
          val resolvedJson      = mkLocal(r.json, schemasBase, manifestFolderURI) // IRI(shexFolderURI).resolve(r.json).uri
          val resolvedShEx      = mkLocal(r.shex, schemasBase, manifestFolderURI) // IRI(shexFolderURI).resolve(r.shex).uri
          val jsonStr   = Source.fromURI(resolvedJson)("UTF-8").mkString
          val schemaStr = Source.fromURI(resolvedShEx)("UTF-8").mkString
          Schema.fromString(schemaStr, "SHEXC", None) match {
            case Right(schema) => {
              decode[Schema](jsonStr) match {
                case Left(err) => result(r.name,false,s"Error parsing Json ${r.json}: $err")
                case Right(expectedSchema) =>
                  if (CompareSchemas.compareSchemas(schema, expectedSchema)) {
                    parse(jsonStr) match {
                      case Left(err) => result(r.name, false,s"Schemas are equal but error parsing Json $jsonStr")
                      case Right(json) => {
                        if (json.equals(schema.asJson)) {
                          result(r.name, true, "JSONs are equal")
                        } else {
                          result(r.name,false,
                            s"Json's are different\nSchema:${schema}\nJson generated: ${schema.asJson.spaces2}\nExpected: ${json.spaces2}"
                          )
                        }
                      }
                    }
                  } else {
                    result(r.name, false, s"Schemas are different. Parsed:\n${schema}\n-----Expected:\n${expectedSchema}")
                  }
              }
            }
            case Left(e) => result(r.name,false, s"Error parsing Schema: ${r.shex}: $e")
          }
        }

/*        case v: Validate => {
          val a = v.action
          val r = for {
            strRdf            <- getContents("data", ep.parentFolder, a.data)
            strSchema         <- getContents("schema", ep.parentFolder, a.schema)
            strShapeMap       <- getContents("shapeMap", ep.parentFolder, a.shapeMap)
            strResultShapeMap <- getContents("resultShapeMap", ep.parentFolder, a.resultShapeMap)
          } yield {
            (strRdf, strSchema, strShapeMap, strResultShapeMap)
          }
          r.fold(
            e => testFail(s"Error: $e"),
            v => {
              val (strRdf, strSchema, strShapeMap, strResultShapeMap) = v
              shouldValidateWithShapeMap(ep.name, strRdf, strSchema, strShapeMap, strResultShapeMap).map(Some(_))
            }
          )
        } */
        case other => result(other.name,false, s"Unsupported type of entry: ${ep.entry}")
      }
    } else EitherT.pure(None)
  }

//  private def testFail[A](msg: String): EitherT[IO, String, A] = EitherT.fromEither(Left(msg))
  private def result[A](name: String, isOk: Boolean, reason: String): EitherT[IO, String, Option[Result]] =
    EitherT.pure(Some(Result(name, isOk, reason)))
  private def fromEither[A](e: Either[String, A]): EitherT[IO, String, A] = EitherT.fromEither(e)
  // private def testInfo(msg: String): EitherT[IO, String, Unit] = EitherT.liftF(IO(println(msg)))

  def getContents(name: String, folder: String, value: Option[IRI]): Either[String, String] = value match {
    case None      => Left(s"No value for $name")
    case Some(iri) => FileUtils.getContents(folder + "/" + iri.str).map(_.toString)
  }

/*  def shouldValidateWithShapeMap(
      name: String,
      rdfStr: String,
      shexStr: String,
      shapeMapStr: String,
      expected: String
  ): EitherT[IO, String, Result] = {
    val validate = for {
      rdf <- RDFAsJenaModel.fromChars(rdfStr, "Turtle")
    } yield true
    validate match {
      case Left(msg) => EitherT.fromEither(s"Error processing RDF: $msg\nRDF=\n$rdfStr".asLeft[(String, Boolean)])
      case Right(v)  => EitherT.pure[IO, String]((name, v))
    }
  } */

  def validateFocusAction(
      fa: FocusAction,
      base: URI,
      v: ValidOrFailureTest,
      shouldValidate: Boolean,
      name: String,
      folderURI: URI
  ): EitherT[IO, String, Option[Result]] = {
    val focus     = fa.focus
    val schemaUri = mkLocal(fa.schema, schemasBase, folderURI)
    val dataUri   = mkLocal(fa.data, schemasBase, folderURI)
    for {
      //_         <- testInfo(s"Validating focusAction: $name")
      schemaStr <- derefUriIO(schemaUri)
      dataStr   <- derefUriIO(dataUri)
      schema    <- fromEither(Schema.fromString(schemaStr, "SHEXC", Some(fa.schema)))
      data      <- fromEither(RDFAsJenaModel.fromChars(dataStr, "TURTLE", Some(fa.data)))
      lbl = fa.shape match {
        case None           => StartMap: ShapeMapLabel
        case Some(i: IRI)   => IRIMapLabel(i)
        case Some(b: BNode) => BNodeMapLabel(b)
        case Some(other) => {
          IRIMapLabel(IRI(s"UnknownLabel"))
        }
      }
      ok <- if (v.traits contains sht_Greedy) {
        result(name, true, "Ignored sht:Greedy")
      } else {
        val shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), data.getPrefixMap, schema.prefixMap)
        for {
          resultShapeMap <- fromEither(
            Validator(schema, ExternalIRIResolver(fa.shapeExterns)).validateShapeMap(data, shapeMap).toEitherS
          )
          ok <- if (resultShapeMap.getConformantShapes(focus) contains lbl) {
            if (shouldValidate) result(name, true, "Conformant shapes match")
            else
              result(name, false, s"Focus $focus conforms to $lbl but should not" ++
                  s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
                  s"${resultShapeMap.getInfo(focus, lbl)}\n" ++
                  s"Schema: ${schema}\n" ++
                  s"Data: ${data}"
              )
          } else {
            if (!shouldValidate) result(name, true, "Doesn't validate as expected")
            else
              result(name, false, 
                s"Focus $focus does not conform to $lbl but should" ++
                  s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
                  s"${resultShapeMap.getInfo(focus, lbl)}\n" ++
                  s"Schema: ${schema}\n" ++
                  s"Data: ${data}"
              )
          }
        } yield ok
      }
    } yield ok
  }

  def validateMapResult(
      mr: MapResultAction,
      base: URI,
      v: ValidOrFailureTest,
      name: String,
      folderURI: URI
  ): EitherT[IO, String, Option[Result]] = {
    v.maybeResult match {
      case None => fail(s"No result specified")
      case Some(resultIRI) => {
        val schemaUri    = mkLocal(mr.schema, validationBase, folderURI)
        val shapeMapUri  = mkLocal(mr.shapeMap, validationBase, folderURI)
        val resultMapUri = mkLocal(resultIRI, validationBase, folderURI)
        val r: EitherT[IO, String, Option[Result]] = for {
          //_             <- testInfo(s"Validating mapResult: $name")
          schemaStr     <- derefUriIO(schemaUri)
          resultMapStr  <- derefUriIO(resultMapUri)
          smapStr       <- derefUriIO(shapeMapUri)
          sm            <- fromEither(ShapeMap.fromJson(smapStr))
          schema        <- fromEither(Schema.fromString(schemaStr, "SHEXC", None))
          fixedShapeMap <- fromEither(ShapeMap.fixShapeMap(sm, RDFAsJenaModel.empty, PrefixMap.empty, PrefixMap.empty))
          dataUri = mkLocal(mr.data, schemasBase, folderURI)
          strData        <- derefUriIO(dataUri)
          data           <- fromEither(RDFAsJenaModel.fromChars(strData, "TURTLE", None))
          resultShapeMap <- fromEither(Validator(schema).validateShapeMap(data, fixedShapeMap).toEitherS)
          jsonResult     <- fromEither(JsonResult.fromJsonString(resultMapStr))
          result <- if (jsonResult.compare(resultShapeMap)) result(name, true, "Json results match")
          else
            result(name,false,
              s"Json results are different. Expected: ${jsonResult.asJson.spaces2}\nObtained: ${resultShapeMap.toString}"
            )
        } yield result
        r
      }
    }
  }

  def derefUriIO(uri: URI): EitherT[IO, String, String] = {
    EitherT(
      IO(
        Either
          .fromTry(
            Try {
              val urlCon = uri.toURL.openConnection()
              urlCon.setConnectTimeout(4000)
              urlCon.setReadTimeout(2000)
              val is = urlCon.getInputStream()
              Source.fromInputStream(is).mkString
            }
          )
          .leftMap(e => s"derefUri($uri): Error: ${e.getMessage}")
      )
    )
  }

}
