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
import es.weso.shex.validator.ExternalIRIResolver
import ManifestPrefixes._

object Utils {

  /**
    * Example:
    * {{{ if iri = IRI("https://raw.githubusercontent.com/shexSpec/shexTest/master/schemas/1dot.shex")
    * baseGlobal = https://raw.githubusercontent.com/shexSpec/shexTest/master/schemas/manifest
    * baseLocal = file:/home/user/src/shapes/shaclex/src/test/resources/shexTest/schemas
    * returns: file:/home/user/src/shapes/shaclex/src/test/resources/shexTest/schemas/1dot.shex
    * }}}
    * @param iri
    * @param baseGlobal
    * @param baseLocal
    * @return
    */
  def mkLocal(iri: IRI, baseGlobal: URI, baseLocal: URI): URI = {
    val parentGlobal = baseGlobal.resolve("..").toString
    val parentLocal = baseLocal.resolve("..").toString
    val resolved = new java.net.URI(iri.uri.toString.replaceFirst(parentGlobal,parentLocal))
    resolved
  }

  val negativeSyntaxBase = new java.net.URI("https://raw.githubusercontent.com/shexSpec/shexTest/master/negativeSyntax/manifest")
  val negativeStructureBase = new java.net.URI("https://raw.githubusercontent.com/shexSpec/shexTest/master/negativeStructure/manifest")
  val schemasBase = new java.net.URI("https://raw.githubusercontent.com/shexSpec/shexTest/master/schemas/manifest")
  val validationBase = new java.net.URI("https://raw.githubusercontent.com/shexSpec/shexTest/master/validation/manifest")

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
    implicit val decodeSchema : Decoder[Schema] = es.weso.shex.implicits.decoderShEx.decodeSchema
    fromES(decode[Schema](jsonStr)(decodeSchema).leftMap(
      e => s"Error decoding JSON string as Schema: ${e.toString}\nJson string:\n${jsonStr}")
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

  def result[A](name: String, isOk: Boolean, reason: String): IO[Option[Result]] =
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

  def testInfo(msg: String, verbose: Boolean): IO[Unit] = 
    if (verbose) IO {
      println(msg); 
    } else IO(())

  def testInfoValue(msg: String, value: Any, verbose: Boolean): IO[Unit] = 
   if (verbose) IO { pprint.log(value, tag = msg); () }
   else IO(())


/* def validateFocusAction(fa: FocusAction,
                          base: URI,
                          v: ValidOrFailureTest,
                          shouldValidate: Boolean,
                          shexFolderURI: URI
                         ): IO[Boolean] = {
    val focus = fa.focus
    val schemaUri = mkLocal(fa.schema,schemasBase,shexFolderURI)
    val dataUri = mkLocal(fa.data,schemasBase,shexFolderURI)
    for {
      schemaStr <- derefUriIO(schemaUri)
      dataStr <- derefUriIO(dataUri)
      schema <- Schema.fromString(schemaStr, "SHEXC", Some(fa.schema))
      ss   <- for {
        res1 <- RDFAsJenaModel.fromChars(dataStr, "TURTLE", Some(fa.data))
        res2 <- RDFAsJenaModel.empty  
        vv <- (res1,res2).tupled.use{case (data,builder) =>
        for {
          dataPrefixMap <- data.getPrefixMap
          resolvedSchema <- ResolvedSchema.resolve(schema, Some(fa.schema))
          lbl = iriLabel(fa)
          rr <- if (v.traits contains sht_Greedy) {
            IO.println(s"Greedy") *> IO(true)
          } else {
            val shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), dataPrefixMap, schema.prefixMap)
            for {
              result <- Validator(resolvedSchema, ExternalIRIResolver(fa.shapeExterns), builder)
                .validateShapeMap(data, shapeMap)
              resultShapeMap <- result.toResultShapeMap
              r <- if (resultShapeMap.getConformantShapes(focus) contains lbl)
                if (shouldValidate) 
                 IO.println(s"Focus $focus conforms to $lbl as expected") *> IO(true)
                else 
                 IO.println(s"Focus $focus conforms to $lbl but should not" ++
                  s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
                  s"${resultShapeMap.getInfo(focus, lbl)}\n" ++
                  s"Schema: ${schema}\n" ++
                  s"Data: ${data}") *>
                 IO(false) 
              else {
                if (!shouldValidate) 
                 IO.println(s"Focus $focus does not conform to $lbl as expected") *>
                 IO(false)
                else 
                 IO.println(s"Focus $focus does not conform to $lbl but should" ++
                  s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
                  s"${resultShapeMap.getInfo(focus, lbl)}\n" ++
                  s"Schema: ${schema}\n" ++
                  s"Data: ${data}") *>
                 IO(false) 
              }
            } yield r
           }
          } yield rr}
      } yield vv 
    } yield ss
  }

  def validateMapResult(mr: MapResultAction,
                        base: URI,
                        v: ValidOrFailureTest,
                        shexFolderURI: URI
                       ): IO[Boolean] = {
    v.maybeResult match {
      case None => IO.println(s"No result specified") *> IO(false)
      case Some(resultIRI) => {
        val schemaUri         = mkLocal(mr.schema, validationBase, shexFolderURI)
        val shapeMapUri       = mkLocal(mr.shapeMap, validationBase, shexFolderURI)
        val resultMapUri      = mkLocal(resultIRI, validationBase, shexFolderURI)
        val r: IO[Boolean] = for {
          schemaStr      <- derefUriIO(schemaUri)
          resultMapStr  <- derefUriIO(resultMapUri)
          smapStr       <- derefUriIO(shapeMapUri)
          sm            <- fromES(ShapeMap.fromJson(smapStr).leftMap(_.toList.mkString("\n")))
          schema        <- Schema.fromString(schemaStr, "SHEXC", None)
          fixedShapeMap <- RDFAsJenaModel.empty.flatMap(_.use(emptyRdf =>
            ShapeMap.fixShapeMap(sm, emptyRdf, PrefixMap.empty, PrefixMap.empty)
          ))
          dataUri = mkLocal(mr.data,schemasBase,shexFolderURI)
          strData        <- derefUriIO(dataUri)
          r           <- for {
            res1 <- RDFAsJenaModel.fromChars(strData, "TURTLE", None)
            res2 <- RDFAsJenaModel.empty
            vv <- (res1, res2).tupled.use{ case (data,builder) =>
           for {
             resolvedSchema <- ResolvedSchema.resolve(schema, None)
             resultVal <- Validator(schema = resolvedSchema, builder = builder).validateShapeMap(data, fixedShapeMap)
             resultShapeMap <- resultVal.toResultShapeMap
             jsonResult     <- fromES(JsonResult.fromJsonString(resultMapStr))
             result <- if (jsonResult.compare(resultShapeMap))
               IO.println(s"Json results match resultShapeMap") *>
               IO(true)
             else
               IO.println(s"Json results are different. Expected: ${jsonResult.asJson.spaces2}\nObtained: ${resultShapeMap.toString}") *>
               IO(false)
           } yield result
        }
          } yield vv 
        } yield r
        r
      }
    }
 } */

 def validateMapResult(
      mr: MapResultAction,
      base: URI,
      v: ValidOrFailureTest,
      name: String,
      folderURI: URI,
      verbose: Boolean
  ): IO[Option[Result]] = {
    v.maybeResult match {
      case None => IO(None) // fail(s"No result specified")
      case Some(resultIRI) => {
        val schemaUri    = mkLocal(mr.schema, validationBase, folderURI)
        val shapeMapUri  = mkLocal(mr.shapeMap, validationBase, folderURI)
        val resultMapUri = mkLocal(resultIRI, validationBase, folderURI)
        val r: IO[Option[Result]] = RDFAsJenaModel.empty.flatMap(_.use(emptyRdf =>

        for {
          _             <- testInfo(s"Validating mapResult: $name", verbose)
          schemaStr     <- derefUriIO(schemaUri)
          _             <- testInfo(s"Schema:\n$schemaStr", verbose)
          resultMapStr  <- derefUriIO(resultMapUri)
          
          smapStr       <- derefUriIO(shapeMapUri)

          _             <- testInfo(s"ShapeMap:\n$smapStr", verbose)
          sm            <- fromES(ShapeMap.fromJson(smapStr).leftMap(s => s"Error parsing shapeMap: $s\nShapeMap:\n$smapStr"))
          schema        <- Schema.fromString(schemaStr, "SHEXC", None)
          resolvedSchema <- ResolvedSchema.resolve(schema, None)
          fixedShapeMap <- ShapeMap.fixShapeMap(sm, emptyRdf, PrefixMap.empty, PrefixMap.empty)
          dataUri = mkLocal(mr.data, schemasBase, folderURI)
          strData        <- derefUriIO(dataUri)
          _             <- testInfo(s"RDF:\n$strData", verbose)
          
          rr      <- for {
            res1 <- RDFAsJenaModel.fromString(strData, "TURTLE", None)
            res2 <- RDFAsJenaModel.empty
            vv <- ( res1, res2).tupled.use{ case (data,builder) =>
           for {
             resultVal <- Validator(schema = resolvedSchema, builder = builder).validateShapeMap(data, fixedShapeMap)
             resultShapeMap <- resultVal.toResultShapeMap

             _             <- testInfo(s"ResultShapeMap:\n$resultShapeMap", verbose)
             _             <- testInfo(s"Expected:\n$resultMapStr", verbose)

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
}