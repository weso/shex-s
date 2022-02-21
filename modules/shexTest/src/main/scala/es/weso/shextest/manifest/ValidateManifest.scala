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
import Reason._
import es.weso.utils._
import es.weso.utils.VerboseLevel._
import scala.concurrent.duration.FiniteDuration

trait RunManifest {

/*  def info(msg: String, verbose: VerboseLevel): IO[Unit] = 
    if (verbose >= Info) IO.println(msg)
    else IO(()) */

  def runManifest(
      name: String,
      folder: String,
      parentFolder: String,
      nameIfSingle: TestSelector,
      ignoreList: List[String],
      withEntry: EntryParam => IO[Option[Result]],
      timeout: FiniteDuration, 
      verbose: VerboseLevel
  ): IO[List[Result]] = {
    val parentFolderURI = Try { Paths.get(parentFolder).normalize.toUri.toString }.getOrElse("")
    val manifestFolder  = s"$parentFolder/$folder"
    val fileName        = s"$manifestFolder/$name.ttl"
    for {
      mf <- RDF2Manifest.read(Paths.get(fileName), "TURTLE", Some(s"$parentFolderURI/$folder/"), true)
      _ <- testInfo(s"Manifest read with ${mf.entries.size} entries", verbose)
      rs <- processManifest(mf, name, manifestFolder, nameIfSingle, ignoreList, withEntry, timeout, verbose)
      _ <-  testInfo(s"Total results: ${rs.size}", verbose)
    } yield rs
  }

  private def runWithTimeout(name: String, action: IO[Option[Result]], timeout: FiniteDuration): IO[Option[Result]] = {
    val timeOutResult = Result(s"Timout: ${name}", false, Timeout(name,timeout))
    action
    .timeoutTo(timeout, IO(Some(timeOutResult)))
    .timed
    .map{ case (time, result) => 
       result.map(_.withTime(time)) 
     } 
  }

  private def processManifest(
      m: ShExManifest,
      name: String,
      parentFolder: String,
      nameIfSingle: TestSelector,
      ignoreList: List[String],
      withEntry: EntryParam => IO[Option[Result]], 
      timeout: FiniteDuration,
      verbose: VerboseLevel
  ): IO[List[Result]] =
    for {
      rs1 <- m.includes.map {
          case (includeNode, manifest) =>
            val folder = Try { Paths.get(includeNode.getLexicalForm).getParent.toString }.getOrElse("")
            runManifest(includeNode.getLexicalForm, folder, parentFolder, nameIfSingle, ignoreList, withEntry, timeout, verbose)
        }.sequence.map(_.flatten)
      _ <- testInfo(s"Results from imports: ${rs1.size}", verbose)
      maybeResults <- m.entries.map(e => 
        runWithTimeout(e.name, withEntry(EntryParam(e, name, parentFolder, nameIfSingle, ignoreList)), timeout)
      ).sequence
      rs2 = maybeResults.flatten[Result]
      _ <- testInfo(s"Results from entries: ${rs2.size}", verbose)
    } yield (rs1 ++ rs2)
}

object ValidateManifest extends RunManifest {

  def showFailed(vs: List[Result], withReason: Boolean): String = {
      vs.map(f => s"${f.name}${if (withReason) s": ${f.reason}" else ""}").mkString("\n") ++
      s"\nNumber of failed tests: ${vs.length}" 
  }

  def parseManifest(
      name: String,
      folderName: String,
      testsFolder: String,
      testSelector: TestSelector,
      ignoreList: List[String],
      timeout: FiniteDuration,
      verbose: VerboseLevel
  ): IO[List[Result]] = {
    testInfo(s"Parse manifest: name: ${name}, folder: $folderName, parentFolder: ${testsFolder}", verbose) *>
    runManifest(name, folderName, testsFolder, testSelector, ignoreList, processEntryValidating(verbose), timeout, verbose)
  } 

  def processEntryValidating(verbose: VerboseLevel)(ep: EntryParam): IO[Option[Result]] = {
    val name = ep.entry.name 
    if (ep.testSelector.matches(name)) {
      if (ep.ignoreList.contains(name)) {
        result(name, true, Ignored(name))        
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
              result(v.name, false, NotImplemented("validate ManifestAction"))
          }
        }
       
        case v: ValidationFailure => {
          v.action match {
            case focusAction: FocusAction => 
             validateFocusAction(focusAction, base, v, false, v.name, folderURI, verbose)
            case mr: MapResultAction      => 
             validateMapResult(mr, base, v, v.name, folderURI, verbose)
            case ma: ManifestAction       => result(v.name, false, NotImplemented("ValidationFailure ManifestAction"))
          }
        }

        case v: NegativeSyntax => negativeSyntax(v, folderURI)
        case v: NegativeStructure => negativeStructure(v,folderURI)
        case r: RepresentationTest => representationTest(r, folderURI)
        case other => result(other.name,false, UnsupportedEntryType(ep.entry))
      }
     }
    } else { 
      None.pure[IO]
    }
  }

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
         case Left(err) => 
           result(repTest.name, false, ErrorParsingJsonStr(jsonStr))
         case Right(json) => {
           if (json.equals(schema.asJson)) {
             result(repTest.name, true, JsonsEqual)
           } else {
             result(repTest.name, false,
               JsonsDifferent(schema, schema.asJson, json)

               
             )
           }
         }
       }
     } else {
       result(repTest.name, false, SchemasDifferent(schema, expectedSchema))
     }
    } yield r
    r
  }

  def negativeSyntax(ns: NegativeSyntax, folderURI: URI): IO[Option[Result]] = {
    val schemaUri    = mkLocal(ns.shex, negativeSyntaxBase, folderURI)
    val r: IO[Option[Result]] = for {
      schemaStr     <- derefUriIO(schemaUri)
      eitherSchema  <- Schema.fromString(schemaStr, "SHEXC", None).attempt
      result <- eitherSchema.fold(s => result(ns.name, true, ParsedFailedAsExpected(s.getMessage())),
                        schema => result(ns.name, false, ParsedOKWithNegativeSyntax(schema, schemaStr)) // s"Parsed OK with ${schema} but should have negative syntax. String: \n${schemaStr}")
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
                       fold(s => result(ns.name, false, SchemaParsedWithNegativeStructure(schema, schemaStr)), // "Schema parsed ok but is not well formed: $s\nSchema string:\n${schemaStr}"),
                        schema => result(ns.name, true, SchemaWellFormed)
                       )
    } yield result
    r
  }

}