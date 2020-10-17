package es.weso.shextest.manifest

import java.net.URI

import es.weso.shex.implicits.decoderShEx.decodeSchema
import es.weso.utils.IOUtils.fromES
import io.circe.{Decoder, Json}
//import es.weso.utils.UriUtils._
import java.nio.file.Paths
import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.PrefixMap
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{BNode, IRI}
import es.weso.shapeMaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMap, _}
import es.weso.shex._
import es.weso.shex.validator.{ExternalIRIResolver, Validator}
import es.weso.shapeMaps._
import es.weso.shex.compact.CompareSchemas
import es.weso.shextest.manifest.Utils._
// import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
//import cats._
import cats.data.EitherT
import cats.implicits._
import cats.effect.IO
import ManifestPrefixes._
//import scala.io._
import io.circe.parser._
import io.circe.syntax._


class ValidationManifestTest extends ValidateManifest {

  // If the following variable is None, it runs all tests
  // Otherwise, it runs only the test whose name is equal to the value of this variable
  val nameIfSingle: Option[String] =
//     Some("1val1IRIREFClosedExtra1_fail-iri2_higher")
     None

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("validationFolder")
//  val shexFolder = conf.getString("shexLocalFolder")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri


  describe("Validation Manifest") {

    val r = RDF2Manifest.read(shexFolder + "/" + "manifest.ttl", "Turtle", Some(shexFolderURI.toString), false)
    r.attempt.unsafeRunSync.fold(e => info(s"Error reading manifest: $e"),
      mf => {
        info(s"Manifest read with ${mf.entries.length} entries")
        for (e <- mf.entries) {
          if (nameIfSingle == None || eq(nameIfSingle.getOrElse(""), e.name)) {
            it(s"Should pass test ${e.name}") {
              e match {
                case r: RepresentationTest => {
                  val resolvedJson = mkLocal(r.json,schemasBase,shexFolderURI)// IRI(shexFolderURI).resolve(r.json).uri
                  val resolvedShEx = mkLocal(r.shex,schemasBase,shexFolderURI)// IRI(shexFolderURI).resolve(r.shex).uri
                  // info(s"Entry: $r with json: ${resolvedJsonIri}")
                  val res: IO[String] = for {
                    jsonStr <- derefUriIO(resolvedJson)
                    schemaStr <- derefUriIO(resolvedShEx)
                    schema <- Schema.fromString(schemaStr, "SHEXC", None)
                    expectedSchema <- decodeSchema(jsonStr)
                    r <- if (CompareSchemas.compareSchemas(schema, expectedSchema)) {
                            parse(jsonStr) match {
                              case Left(err) => ioErr(s"Schemas are equal but error parsing Json $jsonStr")
                              case Right(json) => {
                                if (json.equals(schema2Json(schema))) {
                                  IO("Schemas and Json representations are equal")
                                } else {
                                  ioErr(s"Json's are different\nSchema:${schema}\nJson generated: ${schema.asJson.spaces2}\nExpected: ${json.spaces2}")
                                }
                              }
                          } } else {
                            ioErr(s"Schemas are different. Parsed:\n${schema}\n-----Expected:\n${expectedSchema}")
                          }
                  } yield r
                  res.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"),
                    v => info(s"Passed: $v")
                  )
                }
                case v: ValidationTest => {
                  val base = Paths.get(".").toUri
                  v.action match {
                    case focusAction: FocusAction => validateFocusAction(focusAction,base,v,true)
                    case mr: MapResultAction => validateMapResult(mr,base,v)
                    case ma: ManifestAction => err(s"Not implemented validate ManifestAction yet")
                  }
                }
                case v: ValidationFailure => {
                  val base = Paths.get(".").toUri
                  val r: IO[String] = v.action match {
                    case focusAction: FocusAction => validateFocusAction(focusAction,base,v,false)
                    case mr: MapResultAction => validateMapResult(mr,base,v)
                    case ma: ManifestAction => IO.raiseError(new RuntimeException(s"Not implemented validate ManifestAction yet"))
                  }
                  r.attempt.unsafeRunSync().fold(
                    e => fail(s"Error: ${v.name}: Error: $e"),
                    resultMsg => info(s"ValidationFailure ${v.name} passed")
                  )
                }
              }
            }
          }
       }
     info(s"Manifest read OK: ${mf.entries.length} entries")
    }
  )
  }

  private def schema2Json(schema: Schema): Json = schema.asJson

  private def decodeSchema(jsonStr: String): IO[Schema] = {
    // import es.weso.shex.implicits.decoderShEx.decodeSchema
    implicit val decodeSchema : Decoder[Schema] = es.weso.shex.implicits.decoderShEx.decodeSchema
    fromES(decode[Schema](jsonStr)(decodeSchema).leftMap(
      e => s"Error decoding JSON string as Schema: ${e.toString}\nJson string:\n${jsonStr}")
    )
  }

  private def iriLabel(fa: FocusAction): ShapeMapLabel = fa.shape match {
    case None           => StartMap: ShapeMapLabel
    case Some(i: IRI)   => IRIMapLabel(i)
    case Some(b: BNode) => BNodeMapLabel(b)
    case Some(other) => {
      IRIMapLabel(IRI(s"UnknownLabel"))
    }
  }

  def validateFocusAction(fa: FocusAction,
                          base: URI,
                          v: ValidOrFailureTest,
                          shouldValidate: Boolean
                         ): IO[String] = {
    val focus = fa.focus
    val schemaUri = mkLocal(fa.schema,schemasBase,shexFolderURI)
    val dataUri = mkLocal(fa.data,schemasBase,shexFolderURI)
    for {
      schemaStr <- derefUriIO(schemaUri)
      dataStr <- derefUriIO(dataUri)
      schema <- Schema.fromString(schemaStr, "SHEXC", Some(fa.schema))
      ss   <- (
         RDFAsJenaModel.fromChars(dataStr, "TURTLE", Some(fa.data)),
         RDFAsJenaModel.empty
        ).tupled.use{case (data,builder) =>
        for {
          dataPrefixMap <- data.getPrefixMap
          resolvedSchema <- ResolvedSchema.resolve(schema, Some(fa.schema))
          lbl = iriLabel(fa)
          rr <- if (v.traits contains sht_Greedy) {
            IO(s"Greedy")
          } else {
            val shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), dataPrefixMap, schema.prefixMap)
            for {
              result <- Validator(resolvedSchema, ExternalIRIResolver(fa.shapeExterns), builder)
                .validateShapeMap(data, shapeMap)
              resultShapeMap <- result.toResultShapeMap
              r <- if (resultShapeMap.getConformantShapes(focus) contains lbl)
                if (shouldValidate) IO(s"Focus $focus conforms to $lbl as expected")
                else IO.raiseError(new RuntimeException(s"Focus $focus conforms to $lbl but should not" ++
                  s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
                  s"${resultShapeMap.getInfo(focus, lbl)}\n" ++
                  s"Schema: ${schema}\n" ++
                  s"Data: ${data}"))
              else {
                if (!shouldValidate) IO(s"Focus $focus does not conform to $lbl as expected")
                else IO.raiseError(new RuntimeException(s"Focus $focus does not conform to $lbl but should" ++
                  s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
                  s"${resultShapeMap.getInfo(focus, lbl)}\n" ++
                  s"Schema: ${schema}\n" ++
                  s"Data: ${data}"))
              }
            } yield r
           }
          } yield rr}
    } yield ss
  }

  def validateMapResult(mr: MapResultAction,
                        base: URI,
                        v: ValidOrFailureTest
                       ): IO[String] = {
    v.maybeResult match {
      case None => IO.raiseError(new RuntimeException(s"No result specified"))
      case Some(resultIRI) => {
        val schemaUri         = mkLocal(mr.schema, validationBase, shexFolderURI)
        val shapeMapUri       = mkLocal(mr.shapeMap, validationBase, shexFolderURI)
        val resultMapUri      = mkLocal(resultIRI, validationBase, shexFolderURI)
        val r: IO[String] = for {
          schemaStr      <- derefUriIO(schemaUri)
          resultMapStr  <- derefUriIO(resultMapUri)
          smapStr       <- derefUriIO(shapeMapUri)
          sm            <- fromES(ShapeMap.fromJson(smapStr))
          schema        <- Schema.fromString(schemaStr, "SHEXC", None)
          fixedShapeMap <- RDFAsJenaModel.empty.use(emptyRdf =>
            ShapeMap.fixShapeMap(sm, emptyRdf, PrefixMap.empty, PrefixMap.empty)
          )
          dataUri = mkLocal(mr.data,schemasBase,shexFolderURI)
          strData        <- derefUriIO(dataUri)
          r           <- (
            RDFAsJenaModel.fromChars(strData, "TURTLE", None), 
            RDFAsJenaModel.empty
           ).tupled.use{ case (data,builder) =>
           for {
             resolvedSchema <- ResolvedSchema.resolve(schema, None)
             resultVal <- Validator(schema = resolvedSchema, builder = builder).validateShapeMap(data, fixedShapeMap)
             resultShapeMap <- resultVal.toResultShapeMap
             jsonResult     <- fromES(JsonResult.fromJsonString(resultMapStr))
             result <- if (jsonResult.compare(resultShapeMap))
               IO(s"Json results match resultShapeMap")
             else
               IO.raiseError(new RuntimeException(s"Json results are different. Expected: ${jsonResult.asJson.spaces2}\nObtained: ${resultShapeMap.toString}"))
           } yield result
        }
        } yield r
        r
      }
    }
  }

  private def ioErr[A](msg: String): IO[A] = IO.raiseError(new RuntimeException(msg))

}

