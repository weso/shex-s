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
import matchers.should._
import funspec._


trait RunManifest {

  case class Result(name: String, isOk: Boolean, reason: String)
  case class EntryParam(entry: es.weso.shextest.manifest.Entry, 
    name: String, 
    parentFolder: String, 
    nameIfSingle: Option[String], 
    ignoreList: List[String]
    )
  type EntryProcess = EntryParam => EitherT[IO, String, Option[Result]]

  def runManifest(
      name: String,
      folder: String,
      parentFolder: String,
      nameIfSingle: Option[String],
      ignoreList: List[String],
      withEntry: EntryProcess
  ): EitherT[IO, String, List[Result]] = {
    val parentFolderURI = Try { Paths.get(parentFolder).normalize.toUri.toString }.getOrElse("")
    val manifestFolder  = s"$parentFolder/$folder"
    val fileName        = s"$manifestFolder/$name.ttl"
    for {
      mf <- RDF2Manifest
        .read(fileName, "TURTLE", Some(s"$parentFolderURI/$folder/"), true)
        .leftMap(e => s"Error reading $fileName\nError: $e")
      v <- processManifest(mf, name, manifestFolder, nameIfSingle, ignoreList, withEntry)
    } yield v
  }

  private def processManifest(
      m: ShExManifest,
      name: String,
      parentFolder: String,
      nameIfSingle: Option[String],
      ignoreList: List[String],
      withEntry: EntryProcess
  ): EitherT[IO, String, List[Result]] =
    for {
      vs1 <- m.includes
        .map {
          case (includeNode, manifest) =>
            val folder = Try { Paths.get(includeNode.getLexicalForm).getParent.toString }.getOrElse("")
            runManifest(includeNode.getLexicalForm, folder, parentFolder, nameIfSingle, ignoreList,withEntry)
        }
        .sequence
        .map(_.flatten)
      vs2 <- m.entries.map(e => withEntry(EntryParam(e, name, parentFolder, nameIfSingle, ignoreList))).sequence
    } yield (vs1 ++ vs2.flatten[Result])
}

trait ValidateManifest extends AnyFunSpec with Matchers with TryValues with OptionValues with RunManifest {

  def showFailed(vs: List[Result], withReason: Boolean): String = {
      vs.map(f => s"${f.name}${if (withReason) s": ${f.reason}" else ""}").mkString("\n") ++
      s"\nNumber of failed tests: ${vs.length}" 
  }

  def parseManifest(
      name: String,
      folder: String,
      parentFolder: String,
      nameIfSingle: Option[String],
      ignoreList: List[String],
      verbose: Boolean
  ): Unit = {
    it(s"Should parse manifestTest $folder/$name") {
      val r = runManifest(name, folder, parentFolder, nameIfSingle, ignoreList, processEntryValidating)
      r.value.unsafeRunSync.fold(e => {
        val currentFolder = new java.io.File(".").getCanonicalPath
        fail(s"Error: $e\nCurrent folder: $currentFolder")
    }, vs => {
      val failed = vs.filter(_.isOk == false )
      if (failed.length == 0) 
        info(s"No failures. Number of tests run: ${vs.length}\nIgnored list: ${ignoreList.mkString(",")}")
      else
       fail(s"Failed tests: ${showFailed(failed,verbose)}/${vs.length}")
    })
    }
  }

  def processEntryValidating: EntryProcess = ep => {
    if (ep.nameIfSingle == None || 
        ep.nameIfSingle.getOrElse("") == ep.entry.name
    ) {
      if (ep.ignoreList contains(ep.entry.name)) {
        result(ep.entry.name, true, s"Ignored ${ep.entry.name}")        
      } else {
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
            case mr: MapResultAction      => validateMapResult(mr, base, v, v.name, folderURI)
            case ma: ManifestAction       => result(v.name, false, s"Not implemented validationFailure ManifestAction yet")
          }
        }

        case v: NegativeSyntax => negativeSyntax(v, folderURI)
        case v: NegativeStructure => negativeStructure(v,folderURI)
        case r: RepresentationTest => representationTest(r, folderURI)
        case other => result(other.name,false, s"Unsupported type of entry: ${ep.entry}")
      }
     }
    } else EitherT.pure(None)
  }

//  private def testFail[A](msg: String): EitherT[IO, String, A] = EitherT.fromEither(Left(msg))
  private def result[A](name: String, isOk: Boolean, reason: String): EitherT[IO, String, Option[Result]] =
    EitherT.pure(Some(Result(name, isOk, reason)))

  private def fromEither[A](e: Either[String, A]): EitherT[IO, String, A] = EitherT.fromEither(e)
  // private def testInfo(msg: String): EitherT[IO, String, Unit] = EitherT.liftF(IO(println(msg)))

  def getContents(name: String, folder: String, value: Option[IRI]): EitherT[IO, String, String] = value match {
    case None      => EitherT.fromEither[IO](s"No value for $name".asLeft)
    case Some(iri) => FileUtils.getContents(folder + "/" + iri.str).map(_.toString)
  }

  def eq(s1: String, s2: String): Boolean = s1 == s2

  def err(msg: String): EitherT[IO, String, String] = EitherT.fromEither(msg.asLeft[String])
  def ok(msg: String): EitherT[IO, String, String] = EitherT.fromEither(msg.asRight[String])
  def fromEitherS[A](e: Either[String,A]): EitherT[IO,String,A] = EitherT.fromEither(e)
  def fromIO[A](io: IO[A]): EitherT[IO,String,A] = EitherT.liftF(io)


  def representationTest(repTest: RepresentationTest, folderURI: URI): EitherT[IO, String, Option[Result]] = {
    val resolvedJson      = mkLocal(repTest.json, schemasBase, folderURI) // IRI(shexFolderURI).resolve(r.json).uri
    val resolvedShEx      = mkLocal(repTest.shex, schemasBase, folderURI) // IRI(shexFolderURI).resolve(r.shex).uri
    val r: EitherT[IO,String, Option[Result]] = for {
     jsonStr <- derefUriIO(resolvedJson)
     schemaStr <- derefUriIO(resolvedShEx)
     schema <- EitherT.liftF(Schema.fromString(schemaStr, "SHEXC", None))
     expectedSchema <- fromEither(decode[Schema](jsonStr).leftMap(e => e.toString))
     r <- if (CompareSchemas.compareSchemas(schema, expectedSchema)) {
       parse(jsonStr) match {
         case Left(err) => result(repTest.name, false,s"Schemas are equal but error parsing Json $jsonStr")
         case Right(json) => {
           if (json.equals(schema.asJson)) {
             result(repTest.name, true, "JSONs are equal")
           } else {
             result(repTest.name, false,
               s"Json's are different\nSchema:${schema}\nJson generated: ${schema.asJson.spaces2}\nExpected: ${json.spaces2}"
             )
           }
         }
       }
     } else {
       result(repTest.name, false, s"Schemas are different. Parsed:\n${schema}\n-----Expected:\n${expectedSchema}")
     }
    } yield r
    r
/*     match {
      case Right(schema) => {
         match {
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
    } */
  }

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
      schema    <- EitherT.liftF(Schema.fromString(schemaStr, "SHEXC", Some(fa.schema)))
      data      <- EitherT.liftF(RDFAsJenaModel.fromChars(dataStr, "TURTLE", Some(fa.data)))
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
          resolvedSchema <- EitherT.liftF(ResolvedSchema.resolve(schema, Some(fa.schema)))
          resultVal <- EitherT.liftF(Validator(resolvedSchema, ExternalIRIResolver(fa.shapeExterns)).validateShapeMap(data, shapeMap))
          resultShapeMap <- EitherT.liftF(resultVal.toResultShapeMap)
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
          sm            <- fromEither(ShapeMap.fromJson(smapStr).leftMap(s => s"Error parsing shapeMap: $s\nShapeMap:\n$smapStr"))
          schema        <- EitherT.liftF(Schema.fromString(schemaStr, "SHEXC", None)).leftMap((s: String) => s"Error parsing schema: $s\nSchemaStr:\n $schemaStr")
          resolvedSchema <- EitherT.liftF(ResolvedSchema.resolve(schema, None))
          emptyRdf           <- EitherT.liftF(RDFAsJenaModel.empty)
          fixedShapeMap <- EitherT.liftF(ShapeMap.fixShapeMap(sm, emptyRdf, PrefixMap.empty, PrefixMap.empty)).leftMap((s: String) => s"Error fixing shape map: $s\nShapeMap: $sm")
          dataUri = mkLocal(mr.data, schemasBase, folderURI)
          strData        <- derefUriIO(dataUri)
          data           <- EitherT.liftF(RDFAsJenaModel.fromString(strData, "TURTLE", None))
          resultVal <- EitherT.liftF(Validator(resolvedSchema).validateShapeMap(data, fixedShapeMap)).leftMap((s: String) => s"Error validating: $s")
          resultShapeMap <- EitherT.liftF(resultVal.toResultShapeMap)
          jsonResult     <- fromEither(JsonResult.fromJsonString(resultMapStr).leftMap(s => s"Error parsing JSON result: $s"))
          result <- if (jsonResult.compare(resultShapeMap)) result(name, true, "Json results match")
                    else result(name,false, s"Json results are different. Expected: ${jsonResult.asJson.spaces2}\nObtained: ${resultShapeMap.toString}")
        } yield result
        r
      }
    }
  }

  def negativeSyntax(ns: NegativeSyntax, folderURI: URI): EitherT[IO,String, Option[Result]] = {
    val schemaUri    = mkLocal(ns.shex, negativeSyntaxBase, folderURI)
    val r: EitherT[IO, String, Option[Result]] = for {
      schemaStr     <- derefUriIO(schemaUri)
      eitherSchema  <- EitherT.liftF(Schema.fromString(schemaStr, "SHEXC", None).attempt)
      result <- eitherSchema.fold(s => result(ns.name, true, s.getMessage()),
                        schema => result(ns.name, false, s"Parsed OK with ${schema} but should have negative syntax. String: \n${schemaStr}")
                       ) 
    } yield result
    r
  }

  def negativeStructure(ns: NegativeStructure, folderURI: URI): EitherT[IO,String, Option[Result]] = {
    val schemaUri    = mkLocal(ns.shex, negativeSyntaxBase, folderURI)
    val r: EitherT[IO, String, Option[Result]] = for {
      schemaStr     <- derefUriIO(schemaUri)
      schema        <- EitherT.liftF(Schema.fromString(schemaStr, "SHEXC", None)
                       ).leftMap((e: String) => s"Error reading schema $e\nSchema string:\n${schemaStr}")
      result        <- schema.wellFormed.
                       fold(s => result(ns.name, false, s"Schema parsed ok but is not well formed: $s\nSchema string:\n${schemaStr}"),
                        schema => result(ns.name, true, s"Schema is well formed")
                       )
    } yield result
    r
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