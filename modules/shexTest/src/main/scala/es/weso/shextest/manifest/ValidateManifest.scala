package es.weso.shextest.manifest

import java.net.URI
import java.nio.file.Paths
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shapemaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMap, _}
// import es.weso.shapeMaps.ShapeMap
// import es.weso.utils.FileUtils
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
import es.weso.utils.IOUtils._


trait RunManifest {

  case class Result(name: String, isOk: Boolean, reason: String)
  case class EntryParam(entry: es.weso.shextest.manifest.Entry, 
    name: String, 
    parentFolder: String, 
    nameIfSingle: Option[String], 
    ignoreList: List[String]
    )

  type EntryProcess = EntryParam => IO[Option[Result]]

  def runManifest(
      name: String,
      folder: String,
      parentFolder: String,
      nameIfSingle: Option[String],
      ignoreList: List[String],
      withEntry: EntryProcess
  ): IO[List[Result]] = {
    val parentFolderURI = Try { Paths.get(parentFolder).normalize.toUri.toString }.getOrElse("")
    val manifestFolder  = s"$parentFolder/$folder"
    val fileName        = s"$manifestFolder/$name.ttl"
    for {
      mf <- RDF2Manifest.read(fileName, "TURTLE", Some(s"$parentFolderURI/$folder/"), true)
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
  ): IO[List[Result]] =
    for {
      vs1 <- m.includes
        .map {
          case (includeNode, manifest) =>
            val folder = Try { Paths.get(includeNode.getLexicalForm).getParent.toString }.getOrElse("")
            runManifest(includeNode.getLexicalForm, folder, parentFolder, nameIfSingle, ignoreList, withEntry)
        }
        .sequence
        .map(_.flatten)
      vs2 <- m.entries.map(e => 
       withEntry(EntryParam(e, name, parentFolder, nameIfSingle, ignoreList))
      ).sequence
    } yield (vs1 ++ vs2.flatten[Result])
}

trait ValidateManifest extends RunManifest {

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
  ): IO[List[Result]] = {
    runManifest(name, folder, parentFolder, nameIfSingle, ignoreList, processEntryValidating(verbose))
  }

  def processEntryValidating(verbose: Boolean): EntryProcess = ep => {

    if (ep.nameIfSingle == None || 
        ep.nameIfSingle.getOrElse("") == ep.entry.name
    ) {
      if (ep.ignoreList contains(ep.entry.name)) {
        result(ep.entry.name, true, s"Ignored ${ep.entry.name}")        
      } else {
      val folderURI = Paths.get(ep.parentFolder).normalize.toUri
      val base = Paths.get(".").toUri
      println(s"## Processing entry: ${ep.entry.name}")
       
      ep.entry match {

        case v: ValidationTest => {
          v.action match {
            case focusAction: FocusAction => validateFocusAction(focusAction, base, v, true, v.name, folderURI, verbose)
            case mr: MapResultAction      => validateMapResult(mr, base, v, v.name, folderURI)
            case ma: ManifestAction       =>
              result(v.name, false, s"Not implemented validate ManifestAction yet")
          }
        }
       
        case v: ValidationFailure => {
          v.action match {
            case focusAction: FocusAction => validateFocusAction(focusAction, base, v, false, v.name, folderURI, verbose)
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
    } else None.pure[IO]
  }

  private def result[A](name: String, isOk: Boolean, reason: String): IO[Option[Result]] =
    IO.pure(Some(Result(name, isOk, reason)))

/*  def getContents(name: String, folder: String, value: Option[IRI]): EitherT[IO, String, String] = value match {
    case None      => EitherT.fromEither[IO](s"No value for $name".asLeft)
    case Some(iri) => getContentsEIO(folder + "/" + iri.str)
  }

  private def getContentsEIO(name: String): EitherT[IO, String, String] = {
    EitherT.liftF(FileUtils.getContents(Paths.get(name)))
  } */

  def eq(s1: String, s2: String): Boolean = s1 == s2

  def err(msg: String): EitherT[IO, String, String] = EitherT.fromEither(msg.asLeft[String])
  def ok(msg: String): EitherT[IO, String, String] = EitherT.fromEither(msg.asRight[String])
  def fromEitherS[A](e: Either[String,A]): EitherT[IO,String,A] = EitherT.fromEither(e)
  def fromIO[A](io: IO[A]): EitherT[IO,String,A] = EitherT.liftF(io)


  def representationTest(repTest: RepresentationTest,
                         folderURI: URI): IO[Option[Result]] = {
    val resolvedJson      = mkLocal(repTest.json, schemasBase, folderURI) // IRI(shexFolderURI).resolve(r.json).uri
    val resolvedShEx      = mkLocal(repTest.shex, schemasBase, folderURI) // IRI(shexFolderURI).resolve(r.shex).uri
    val r: IO[Option[Result]] = for {
     jsonStr <- derefUriIO(resolvedJson)
     schemaStr <- derefUriIO(resolvedShEx)
     schema <- Schema.fromString(schemaStr, "SHEXC", None)
     expectedSchema <- fromES(decode[Schema](jsonStr).leftMap(e => e.toString))
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
  }

  def testInfo(msg: String, verbose: Boolean): IO[Unit] = 
    if (verbose) IO {
      println(msg); 
    } else IO(())

  def testInfoValue(msg: String, value: Any, verbose: Boolean): IO[Unit] = 
   if (verbose) IO { pprint.log(value, tag = msg); () }
   else IO(())

  def validateFocusAction(
      fa: FocusAction,
      base: URI,
      v: ValidOrFailureTest,
      shouldValidate: Boolean,
      name: String,
      folderURI: URI,
      verbose: Boolean
  ): IO[Option[Result]] = {
    val focus     = fa.focus
    val schemaUri = mkLocal(fa.schema, schemasBase, folderURI)
    val dataUri   = mkLocal(fa.data, schemasBase, folderURI)
    for {
      //_         <- testInfo(s"Validating focusAction: $name",verbose)
      schemaStr <- derefUriIO(schemaUri)
      //_         <- testInfo(s"schemaStr:\n$schemaStr\n-----end schemaStr\nNest step: deref: $dataUri", verbose)
      dataStr   <- derefUriIO(dataUri)
      //_         <- testInfo(s"dataStr:\n$dataStr\n-----end dataStr", verbose)
      schema    <- Schema.fromString(schemaStr, "SHEXC", Some(fa.schema))
      //_         <- testInfoValue(s"schema", schema.asJson.spaces2, verbose)
      result      <- for {
        res1 <- RDFAsJenaModel.fromChars(dataStr, "TURTLE", Some(fa.data))
        res2 <- RDFAsJenaModel.empty
        vv <- (res1,res2).tupled.use{ case (data, builder) =>
       for {
         dataPrefixMap <- data.getPrefixMap
         // _         <- testInfoValue(s"data", data, verbose)
         lbl = getLabel(fa)
         // _         <- testInfoValue(s"label", lbl, verbose)
         ok <- if (v.traits contains sht_Greedy) {
           result(name, true, "Ignored sht:Greedy")
         } else {
           val shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), dataPrefixMap, schema.prefixMap)
           for {
             // _         <- testInfoValue(s"shapeMap", shapeMap, verbose)
             resolvedSchema <- ResolvedSchema.resolve(schema, Some(fa.schema))
             // _         <- testInfoValue(s"resolvedSchema", resolvedSchema, verbose)
             resultVal <- Validator(schema = resolvedSchema, 
                externalResolver = ExternalIRIResolver(fa.shapeExterns),
                builder = builder
               ).validateShapeMap(data, shapeMap)
             resultShapeMap <- resultVal.toResultShapeMap
             _         <- testInfoValue(s"resultShapeMap", resultShapeMap, verbose)
             ok <- if (resultShapeMap.getConformantShapes(focus) contains lbl) {
               if (shouldValidate) result(name, true, "Conformant shapes match")
               else
                 result(name, false, s"Focus $focus conforms to $lbl but should not" ++
                   s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
                   s"${resultShapeMap.getInfo(focus, lbl)}\n" // ++
                   // s"Schema: ${schema}\n" ++
                   // s"Data: ${data}"
                 )
             } else {
               if (!shouldValidate) result(name, true, "Doesn't validate as expected")
               else
                 result(name, false,
                   s"Focus $focus does not conform to $lbl but should" ++
                     s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
                     s"${resultShapeMap.getInfo(focus, lbl)}\n" /* ++
                     s"Schema: ${schema}\n" ++
                     s"Data: ${data}" */
                 )
             }
           } yield ok
         }
        } yield ok }
      } yield vv 
     } yield result
  }

  private def getLabel(fa: FocusAction): ShapeMapLabel =
    fa.shape match {
    case None           => StartMap: ShapeMapLabel
    case Some(i: IRI)   => IRIMapLabel(i)
    case Some(b: BNode) => BNodeMapLabel(b)
    case Some(other) => {
      IRIMapLabel(IRI(s"UnknownLabel"))
    }
  }

  def validateMapResult(
      mr: MapResultAction,
      base: URI,
      v: ValidOrFailureTest,
      name: String,
      folderURI: URI
  ): IO[Option[Result]] = {
    v.maybeResult match {
      case None => IO(None) // fail(s"No result specified")
      case Some(resultIRI) => {
        val schemaUri    = mkLocal(mr.schema, validationBase, folderURI)
        val shapeMapUri  = mkLocal(mr.shapeMap, validationBase, folderURI)
        val resultMapUri = mkLocal(resultIRI, validationBase, folderURI)
        val r: IO[Option[Result]] = RDFAsJenaModel.empty.flatMap(_.use(emptyRdf =>
          for {
          //_             <- testInfo(s"Validating mapResult: $name")
          schemaStr     <- derefUriIO(schemaUri)
          resultMapStr  <- derefUriIO(resultMapUri)
          smapStr       <- derefUriIO(shapeMapUri)
          sm            <- fromES(ShapeMap.fromJson(smapStr).leftMap(s => s"Error parsing shapeMap: $s\nShapeMap:\n$smapStr"))
          schema        <- Schema.fromString(schemaStr, "SHEXC", None)
          resolvedSchema <- ResolvedSchema.resolve(schema, None)
          fixedShapeMap <- ShapeMap.fixShapeMap(sm, emptyRdf, PrefixMap.empty, PrefixMap.empty)
          dataUri = mkLocal(mr.data, schemasBase, folderURI)
          strData        <- derefUriIO(dataUri)
          rr      <- for {
            res1 <- RDFAsJenaModel.fromString(strData, "TURTLE", None)
            res2 <- RDFAsJenaModel.empty
            vv <- ( res1, res2).tupled.use{ case (data,builder) =>
           for {
             resultVal <- Validator(schema = resolvedSchema, builder = builder).validateShapeMap(data, fixedShapeMap)
             resultShapeMap <- resultVal.toResultShapeMap
             jsonResult     <- fromES(JsonResult.fromJsonString(resultMapStr))
             r <- if (jsonResult.compare(resultShapeMap))
                    result(name, true, "Json results match")
                  else
                    result(name,false, s"Json results are different. Expected: ${jsonResult.asJson.spaces2}\nObtained: ${resultShapeMap.toString}")
           } yield r }
          } yield vv 
          } yield rr))
        r
      }
    }
  }

  def negativeSyntax(ns: NegativeSyntax, folderURI: URI): IO[Option[Result]] = {
    val schemaUri    = mkLocal(ns.shex, negativeSyntaxBase, folderURI)
    val r: IO[Option[Result]] = for {
      schemaStr     <- derefUriIO(schemaUri)
      eitherSchema  <- Schema.fromString(schemaStr, "SHEXC", None).attempt
      result <- eitherSchema.fold(s => result(ns.name, true, s.getMessage()),
                        schema => result(ns.name, false, s"Parsed OK with ${schema} but should have negative syntax. String: \n${schemaStr}")
                       ) 
    } yield result
    r
  }

  def negativeStructure(ns: NegativeStructure, folderURI: URI): IO[Option[Result]] = {
    val schemaUri    = mkLocal(ns.shex, negativeSyntaxBase, folderURI)
    val r: IO[Option[Result]] = for {
      schemaStr     <- derefUriIO(schemaUri)
      schema        <- Schema.fromString(schemaStr, "SHEXC", None)
      result        <- schema.wellFormed.
                       fold(s => result(ns.name, false, s"Schema parsed ok but is not well formed: $s\nSchema string:\n${schemaStr}"),
                        schema => result(ns.name, true, s"Schema is well formed")
                       )
    } yield result
    r
  }


  def derefUriIO(uri: URI): IO[String] = {
    Try {
      val urlCon = uri.toURL.openConnection()
      urlCon.setConnectTimeout(4000)
      urlCon.setReadTimeout(2000)
      val is = urlCon.getInputStream()
      Source.fromInputStream(is).mkString
     }.fold(
      e => IO.raiseError(new RuntimeException(s"derefUri($uri): Error: ${e.getMessage}")),
      IO(_)
    )
  }

}