package es.weso.shextest.manifest

import java.net.URI
import java.nio.file.Paths
import scala.util.{Either, Left, Right, Try}
import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import es.weso.shex._
import es.weso.shex.compact.CompareSchemas
import es.weso.shextest.manifest.Utils._
import es.weso.shex.implicits.encoderShEx._
import io.circe.parser._
import io.circe.syntax._


trait RunManifest {

  def info(msg: String, verbose: Boolean): IO[Unit] = 
    if (verbose) {
      IO.println(msg)
    } else IO(())

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
      withEntry: EntryProcess,
      verbose: Boolean
  ): IO[List[Result]] = {
    val parentFolderURI = Try { Paths.get(parentFolder).normalize.toUri.toString }.getOrElse("")
    val manifestFolder  = s"$parentFolder/$folder"
    val fileName        = s"$manifestFolder/$name.ttl"
    for {
      mf <- RDF2Manifest.read(Paths.get(fileName), "TURTLE", Some(s"$parentFolderURI/$folder/"), true)
      _ <- info(s"Manifest read with ${mf.entries.size} entries", verbose)
      rs <- processManifest(mf, name, manifestFolder, nameIfSingle, ignoreList, withEntry, verbose)
      _ <-  info(s"Total results: ${rs.size}", verbose)
    } yield rs
  }

  private def processManifest(
      m: ShExManifest,
      name: String,
      parentFolder: String,
      nameIfSingle: Option[String],
      ignoreList: List[String],
      withEntry: EntryProcess, 
      verbose: Boolean
  ): IO[List[Result]] =
    for {
      rs1 <- m.includes.map {
          case (includeNode, manifest) =>
            val folder = Try { Paths.get(includeNode.getLexicalForm).getParent.toString }.getOrElse("")
            runManifest(includeNode.getLexicalForm, folder, parentFolder, nameIfSingle, ignoreList, withEntry, verbose)
        }.sequence.map(_.flatten)
      _ <- info(s"Results from imports: ${rs1.size}", verbose)
      maybeResults <- m.entries.map(e => withEntry(EntryParam(e, name, parentFolder, nameIfSingle, ignoreList))).sequence
      rs2 = maybeResults.flatten[Result]
      _ <- info(s"Results from entries: ${rs2.size}", verbose)
    } yield (rs1 ++ rs2)
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
    info(s"Parse manifest: name: ${name}, parentFolder: ${parentFolder}", verbose) *>
    runManifest(name, folder, parentFolder, nameIfSingle, ignoreList, processEntryValidating(verbose), verbose)
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

      ep.entry match {

        case v: ValidationTest => {
          v.action match {
            case focusAction: FocusAction => 
             validateFocusAction(focusAction, base, v, true, v.name, folderURI, verbose)
            case mr: MapResultAction      => 
             validateMapResult(mr, base, v, v.name, folderURI, verbose)
            case ma: ManifestAction       =>
              result(v.name, false, s"Not implemented validate ManifestAction yet")
          }
        }
       
        case v: ValidationFailure => {
          v.action match {
            case focusAction: FocusAction => 
             validateFocusAction(focusAction, base, v, false, v.name, folderURI, verbose)
            case mr: MapResultAction      => 
             validateMapResult(mr, base, v, v.name, folderURI, verbose)
            case ma: ManifestAction       => result(v.name, false, s"Not implemented validationFailure ManifestAction yet")
          }
        }

        case v: NegativeSyntax => negativeSyntax(v, folderURI)
        case v: NegativeStructure => negativeStructure(v,folderURI)
        case r: RepresentationTest => representationTest(r, folderURI)
        case other => result(other.name,false, s"Unsupported type of entry: ${ep.entry}")
      }
     }
    } else { 
      None.pure[IO]
    }
  }



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
    // implicit val decodeSchema : Decoder[Schema] = es.weso.shex.implicits.decoderShEx.decodeSchema                           
    val resolvedJson      = mkLocal(repTest.json, schemasBase, folderURI) // IRI(shexFolderURI).resolve(r.json).uri
    val resolvedShEx      = mkLocal(repTest.shex, schemasBase, folderURI) // IRI(shexFolderURI).resolve(r.shex).uri
    val r: IO[Option[Result]] = for {
     jsonStr <- derefUriIO(resolvedJson)
     schemaStr <- derefUriIO(resolvedShEx)
     schema <- Schema.fromString(schemaStr, "SHEXC", None)
     expectedSchema <- jsonStr2Schema(jsonStr)// fromES(decode[Schema](jsonStr).leftMap(e => e.toString))
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

/*  private def validateMapResultDel(
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
  } */

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

}