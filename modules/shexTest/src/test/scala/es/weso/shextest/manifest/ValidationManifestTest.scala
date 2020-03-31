package es.weso.shextest.manifest

import java.net.URI
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
//import es.weso.shex.compact.CompareSchemas
import es.weso.shextest.manifest.Utils._
//import es.weso.shex.implicits.decoderShEx._
//import es.weso.shex.implicits.encoderShEx._
//import cats._
import cats.data.EitherT
import cats.implicits._
import cats.effect.IO
import ManifestPrefixes._
//import scala.io._
//import io.circe.parser._
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

  println(s"ValidationManifest")

  describe("ValidationManifest") {

    val r = RDF2Manifest.read(shexFolder + "/" + "manifest.ttl", "Turtle", Some(shexFolderURI.toString), false)
    r.fold(e => println(s"Error reading manifest: $e"),
      mf => {
        println(s"Manifest read with ${mf.entries.length} entries")
        for (e <- mf.entries) {
          if (nameIfSingle == None || eq(nameIfSingle.getOrElse(""), e.name)) {
            it(s"Should pass test ${e.name}") {
              e match {
                case r: RepresentationTest => ??? /*{
                  val resolvedJson = mkLocal(r.json,schemasBase,shexFolderURI)// IRI(shexFolderURI).resolve(r.json).uri
                  val resolvedShEx = mkLocal(r.shex,schemasBase,shexFolderURI)// IRI(shexFolderURI).resolve(r.shex).uri
                  // info(s"Entry: $r with json: ${resolvedJsonIri}")
                  val res: EitherT[IO, String, String] = for {
                    jsonStr <- derefUriIO(resolvedJson)
                    schemaStr <- derefUriIO(resolvedShEx)
                    schema <- EitherT.liftF(Schema.fromString(schemaStr, "SHEXC", None))
                    expectedSchema <- fromEither(decode[Schema](jsonStr).leftMap(e => e.toString))
                    r <- if (CompareSchemas.compareSchemas(schema, expectedSchema)) {
                            parse(jsonStr) match {
                              case Left(err) => s"Schemas are equal but error parsing Json $jsonStr".asLeft
                              case Right(json) => {
                                if (json.equals(schema.asJson)) {
                                  "Schemas and Json representations are equal".asRight
                                } else {
                                  s"Json's are different\nSchema:${schema}\nJson generated: ${schema.asJson.spaces2}\nExpected: ${json.spaces2}".asLeft
                                }
                              }
                          } } else {
                            s"Schemas are different. Parsed:\n${schema}\n-----Expected:\n${expectedSchema}".asLeft
                          }
                  } yield r
                  res.value.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"), 
                    v => info(s"Passed: $v")
                  )
                } */
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
                  val r: EitherT[IO,String,String] = v.action match {
                    case focusAction: FocusAction => validateFocusAction(focusAction,base,v,false)
                    case mr: MapResultAction => validateMapResult(mr,base,v)
                    case ma: ManifestAction => err(s"Not implemented validate ManifestAction yet")
                  }
                  r.fold(e => fail(s"Error: ${v.name}: Error: $e"), resultMsg => {
                    info(s"ValidationFailure ${v.name} passed")
                  })
                }
              }
            }
          }
       }
     info(s"Manifest read OK: ${mf.entries.length} entries")
    }
  )
  }

  def validateFocusAction(fa: FocusAction,
                          base: URI,
                          v: ValidOrFailureTest,
                          shouldValidate: Boolean
                         ): EitherT[IO, String, String] = {
    val focus = fa.focus
    val schemaUri = mkLocal(fa.schema,schemasBase,shexFolderURI)
    val dataUri = mkLocal(fa.data,schemasBase,shexFolderURI)
    for {
      schemaStr <- derefUriIO(schemaUri)
      dataStr <- derefUriIO(dataUri)
      schema <- fromIO(Schema.fromString(schemaStr, "SHEXC", Some(fa.schema)))
      data   <- fromIO(RDFAsJenaModel.fromChars(dataStr, "TURTLE", Some(fa.data)))
      resolvedSchema <- fromIO(ResolvedSchema.resolve(schema, Some(fa.schema)))
      lbl = fa.shape match {
        case None           => StartMap: ShapeMapLabel
        case Some(i: IRI)   => IRIMapLabel(i)
        case Some(b: BNode) => BNodeMapLabel(b)
        case Some(other) => {
          IRIMapLabel(IRI(s"UnknownLabel"))
        }
      }
      res <- if (v.traits contains sht_Greedy) {
        ok(s"Greedy")
      } else {
        val shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), data.getPrefixMap, schema.prefixMap)
        for {
          result <- fromIO(Validator(resolvedSchema, ExternalIRIResolver(fa.shapeExterns))
            .validateShapeMap(data, shapeMap))
          resultShapeMap <- fromIO(result.toResultShapeMap)  
          r <- if (resultShapeMap.getConformantShapes(focus) contains lbl)
            if (shouldValidate) ok(s"Focus $focus conforms to $lbl as expected")
            else err(s"Focus $focus conforms to $lbl but should not" ++
                      s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
                      s"${resultShapeMap.getInfo(focus, lbl)}\n" ++
                      s"Schema: ${schema}\n" ++
                      s"Data: ${data}")
          else {
            if (!shouldValidate) ok(s"Focus $focus does not conforms to $lbl as expected")
            else err(s"Focus $focus does not conform to $lbl but should" ++
              s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
              s"${resultShapeMap.getInfo(focus, lbl)}\n" ++
              s"Schema: ${schema}\n" ++
              s"Data: ${data}")
          }
        } yield r
      }
    } yield res
  } 

  def validateMapResult(mr: MapResultAction,
                        base: URI,
                        v: ValidOrFailureTest
                       ): EitherT[IO, String, String] = {
    v.maybeResult match {
      case None => EitherT.fromEither(s"No result specified".asLeft[String])
      case Some(resultIRI) => {
        val schemaUri         = mkLocal(mr.schema, validationBase, shexFolderURI)
        val shapeMapUri       = mkLocal(mr.shapeMap, validationBase, shexFolderURI)
        val resultMapUri      = mkLocal(resultIRI, validationBase, shexFolderURI)
        val r: EitherT[IO, String, String] = for {
          schemaStr      <- derefUriIO(schemaUri)
          resultMapStr  <- derefUriIO(resultMapUri)
          smapStr       <- derefUriIO(shapeMapUri)
          sm            <- fromEitherS(ShapeMap.fromJson(smapStr))
          schema        <- fromIO(Schema.fromString(schemaStr, "SHEXC", None))
          emptyRdf      <- fromIO(RDFAsJenaModel.empty)
          fixedShapeMap <- fromIO(ShapeMap.fixShapeMap(sm, emptyRdf, PrefixMap.empty, PrefixMap.empty))
          dataUri = mkLocal(mr.data,schemasBase,shexFolderURI)
          strData        <- derefUriIO(dataUri)
          data           <- fromIO(RDFAsJenaModel.fromChars(strData, "TURTLE", None))
          resolvedSchema <- fromIO(ResolvedSchema.resolve(schema, None))
          resultVal <- fromIO(Validator(resolvedSchema).validateShapeMap(data, fixedShapeMap))
          resultShapeMap <- fromIO(resultVal.toResultShapeMap)
          jsonResult     <- fromEitherS(JsonResult.fromJsonString(resultMapStr))
          result <- if (jsonResult.compare(resultShapeMap)) 
            ok(s"Json results match resultShapeMap")
          else
            err(s"Json results are different. Expected: ${jsonResult.asJson.spaces2}\nObtained: ${resultShapeMap.toString}")
        } yield result 
        r
      }
    }
  }
}

