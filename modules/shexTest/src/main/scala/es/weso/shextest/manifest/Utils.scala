package es.weso.shextest.manifest
import java.net.URI

import es.weso.rdf.nodes.IRI
import cats.effect.IO
import scala.util.Try
import scala.io.Source
import es.weso.shex.Schema
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import cats.implicits._
import es.weso.shex.implicits.encoderShEx._
// import io.circe.syntax._

import es.weso.utils.IOUtils._
import es.weso.shapemaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMap, _}
import es.weso.rdf.nodes.BNode
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex.ResolvedSchema
import es.weso.shex.validator.Validator
import es.weso.rdf.PrefixMap
import es.weso.shex.validator.ExternalResolver._
import ManifestPrefixes._
import es.weso.shex.validator.ExternalResolver
import Reason._
import es.weso.utils._
import es.weso.utils.VerboseLevel.{Info => InfoLevel, _}
import java.nio.file.Path
import java.nio.file.Paths

object Utils {

  /** Example: {{{ if iri = IRI("https://raw.githubusercontent.com/shexSpec/shexTest/master/schemas/1dot.shex")
    * baseGlobal = https://raw.githubusercontent.com/shexSpec/shexTest/master/schemas/manifest baseLocal =
    * file:/home/user/src/shapes/shaclex/src/test/resources/shexTest/schemas returns:
    * file:/home/user/src/shapes/shaclex/src/test/resources/shexTest/schemas/1dot.shex }}}
    * @param iri
    * @param baseGlobal
    * @param baseLocal
    * @return
    */
  def mkLocal(iri: IRI, baseGlobal: URI, baseLocal: URI): URI = {
    val parentGlobal = baseGlobal.resolve("..").toString
    val parentLocal  = baseLocal.resolve("..").toString
    val resolved     = new java.net.URI(iri.uri.toString.replaceFirst(parentGlobal, parentLocal))
    resolved
  }

  val negativeSyntaxBase =
    new java.net.URI("https://raw.githubusercontent.com/shexSpec/shexTest/master/negativeSyntax/manifest")
  val negativeStructureBase =
    new java.net.URI("https://raw.githubusercontent.com/shexSpec/shexTest/master/negativeStructure/manifest")
  val schemasBase = new java.net.URI("https://raw.githubusercontent.com/shexSpec/shexTest/master/schemas/manifest")
  val validationBase =
    new java.net.URI("https://raw.githubusercontent.com/shexSpec/shexTest/master/validation/manifest")

  // TODO: Replace by http4s client
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

  def jsonStr2Schema(jsonStr: String): IO[Schema] = {
    // import es.weso.shex.implicits.decoderShEx.decodeSchema
    implicit val decodeSchema: Decoder[Schema] = es.weso.shex.implicits.decoderShEx.decodeSchema
    fromES(
      decode[Schema](jsonStr)(decodeSchema).leftMap(e =>
        s"Error decoding JSON string as Schema: ${e.toString}\nJson string:\n${jsonStr}"
      )
    )
  }

  def ioErr[A](msg: String): IO[A] = IO.raiseError(new RuntimeException(msg))

  def schema2Json(schema: Schema): Json = schema.asJson

  def iriLabel(fa: FocusAction): ShapeMapLabel = fa.shape match {
    case None           => StartMap: ShapeMapLabel
    case Some(i: IRI)   => IRIMapLabel(i)
    case Some(b: BNode) => BNodeMapLabel(b)
    case Some(other) => {
      IRIMapLabel(IRI(s"UnknownLabel"))
    }
  }

  def validateFocusAction(
      fa: FocusAction,
      base: URI,
      v: ValidOrFailureTest,
      shouldValidate: Boolean,
      name: String,
      folderURI: URI,
      assumeLocal: Option[(IRI, Path)],
      verbose: VerboseLevel
  ): IO[Option[Result]] = {
    val focus     = fa.focus
    val schemaUri = mkLocal(fa.schema, schemasBase, folderURI)
    val dataUri   = mkLocal(fa.data, schemasBase, folderURI)
    for {
      _         <- testInfo(s"focusAction:  $name", verbose)
      _         <- testInfo(s"SchemasBase:  $schemasBase", verbose)
      _         <- testInfo(s"folderURI:    $folderURI", verbose)
      _         <- testInfo(s"Schema uri:   $schemaUri", verbose)
      _         <- testInfo(s"Data uri:     $dataUri", verbose)
      _         <- testInfo(s"FOCUS: ${fa.focus}", verbose)
      _         <- testInfo(s"Shape: ${fa.shape.getOrElse("<>")}", verbose)
      _         <- testInfo(s"ShouldValidate: ${shouldValidate}", verbose)
      schemaStr <- derefUriIO(schemaUri)
//      _         <- testInfo(s"schemaStr:\n$schemaStr\n-----end schemaStr\nNest step: deref: $dataUri", verbose)
      dataStr <- derefUriIO(dataUri)
//      _         <- testInfo(s"dataStr:\n$dataStr\n-----end dataStr", verbose)
      schema <- Schema.fromString(schemaStr, "SHEXC", Some(fa.schema))
      // _         <- testInfoValue(s"schema", schema.asJson.spaces2, verbose)
      result <- for {
        res1 <- RDFAsJenaModel.fromChars(dataStr, "TURTLE", Some(fa.data))
        res2 <- RDFAsJenaModel.empty
        vv <- (res1, res2).tupled.use { case (data, builder) =>
          for {
            dataPrefixMap <- data.getPrefixMap
            // _         <- testInfoValue(s"data", data, verbose)
            lbl = getLabel(fa)
            // _         <- testInfoValue(s"label", lbl, verbose)
            ok <-
              if (v.traits contains sht_Greedy) {
                result(name, true, Ignored("Ignored sht:Greedy"))
              } else {
                val shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), dataPrefixMap, schema.prefixMap)
                for {
                  // _         <- testInfoValue(s"shapeMap", shapeMap, verbose)
                  resolvedSchema <- ResolvedSchema.resolve(schema, Some(fa.schema), verbose, assumeLocal)
                  // _         <- testInfoValue(s"resolvedSchema", resolvedSchema, verbose)
                  externalResolver: ExternalResolver = fa.shapeExterns.fold[ExternalResolver](
                    ExternalResolver.NoAction
                  )(ExternalResolver.ExternalIRIResolver(_, verbose))
                  validator = Validator(schema = resolvedSchema, externalResolver = externalResolver, builder = builder)
                  resultVal      <- validator.validateShapeMap(data, shapeMap, verbose)
                  resultShapeMap <- resultVal.toResultShapeMap
                  _ <- testInfo(s"resultShapeMap: ${resultShapeMap.showShapeMap(verbose.asBoolean)}", verbose)
                  ok <-
                    if (resultShapeMap.getConformantShapes(focus) contains lbl) {
                      if (shouldValidate)
                        result(name, true, ConformantMatch(focus, lbl, resultShapeMap))
                      else
                        result(name, false, ConformsButShoudnt(focus, lbl, dataStr, schemaStr, resultShapeMap))
                    } else {
                      if (!shouldValidate) result(name, true, DoesntValidateAsExpected)
                      else
                        result(name, false, DoesntConformButShould(focus, lbl, dataStr, schemaStr, resultShapeMap))
                    }
                } yield ok
              }
          } yield ok
        }
      } yield vv
    } yield result
  }

  def result[A](name: String, isOk: Boolean, reason: Reason): IO[Option[Result]] =
    IO.pure(Some(Result(name, isOk, reason)))

  private def getLabel(fa: FocusAction): ShapeMapLabel =
    fa.shape match {
      case None           => StartMap: ShapeMapLabel
      case Some(i: IRI)   => IRIMapLabel(i)
      case Some(b: BNode) => BNodeMapLabel(b)
      case Some(other) => {
        IRIMapLabel(IRI(s"UnknownLabel"))
      }
    }

  def testInfo(msg: String, verboseLevel: VerboseLevel): IO[Unit] =
    verboseLevel.info(msg)

  def testDebug(msg: String, verboseLevel: VerboseLevel): IO[Unit] =
    verboseLevel.debug(msg)

  def testInfoValue(msg: String, value: Any, verboseLevel: VerboseLevel): IO[Unit] =
    if (verboseLevel > InfoLevel) IO { pprint.log(value, tag = msg); () }
    else IO(())

  def validateMapResult(
      mr: MapResultAction,
      base: URI,
      v: ValidOrFailureTest,
      name: String,
      folderURI: URI,
      verbose: VerboseLevel
  ): IO[Option[Result]] = {
    v.maybeResult match {
      case None => IO(None)
      case Some(resultIRI) => {
        val schemaUri    = mkLocal(mr.schema, validationBase, folderURI)
        val shapeMapUri  = mkLocal(mr.shapeMap, validationBase, folderURI)
        val resultMapUri = mkLocal(resultIRI, validationBase, folderURI)
        val r: IO[Option[Result]] = RDFAsJenaModel.empty.flatMap(
          _.use(emptyRdf =>
            for {
              _            <- testInfo(s"Validating mapResult: $name", verbose)
              schemaStr    <- derefUriIO(schemaUri)
              _            <- testInfo(s"Schema URI:\n$schemaStr", verbose)
              resultMapStr <- derefUriIO(resultMapUri)

              smapStr <- derefUriIO(shapeMapUri)

              _  <- testInfo(s"ShapeMap:\n$smapStr", verbose)
              sm <- fromES(ShapeMap.fromJson(smapStr).leftMap(s => s"Error parsing shapeMap: $s\nShapeMap:\n$smapStr"))
              schema         <- Schema.fromString(schemaStr, "SHEXC", None)
              resolvedSchema <- ResolvedSchema.resolve(schema, None, verbose)
              fixedShapeMap  <- ShapeMap.fixShapeMap(sm, emptyRdf, PrefixMap.empty, PrefixMap.empty)
              dataUri = mkLocal(mr.data, schemasBase, folderURI)
              strData <- derefUriIO(dataUri)
              _       <- testInfo(s"RDF:\n$strData", verbose)

              rr <- for {
                res1 <- RDFAsJenaModel.fromString(strData, "TURTLE", None)
                res2 <- RDFAsJenaModel.empty
                vv <- (res1, res2).tupled.use { case (data, builder) =>
                  for {
                    resultVal <- Validator(schema = resolvedSchema, builder = builder)
                      .validateShapeMap(data, fixedShapeMap, verbose)
                    resultShapeMap <- resultVal.toResultShapeMap
                    _              <- testInfo(s"ResultShapeMap obtained:$resultShapeMap", verbose)
                    _              <- testInfo(s"Expected:\n$resultMapStr", verbose)
                    jsonResult     <- fromES(JsonResult.fromJsonString(resultMapStr))
                    _              <- testInfo(s"jsonResult expected:\n$jsonResult", verbose)
                    r <-
                      if (jsonResult.compare(resultShapeMap))
                        result(name, true, JsonResultsMatch(jsonResult))
                      else
                        result(name, false, JsonResultsDifferent(resultShapeMap, jsonResult))
                  } yield r
                }
              } yield vv
            } yield rr
          )
        )
        r
      }
    }
  }
}
