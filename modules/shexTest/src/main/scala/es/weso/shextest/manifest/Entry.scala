package es.weso.shextest.manifest

import es.weso.rdf.nodes._
import ManifestPrefixes._
import es.weso.utils.testsuite._
import es.weso.shextest.manifest.Utils._
import java.net.URI
import cats.effect.IO
//import cats._
import cats.implicits._
import es.weso.shex.Schema
import es.weso.shex.compact.CompareSchemas
import io.circe.parser._
import java.nio.file.Paths
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex.ResolvedSchema
import es.weso.shapemaps.FixedShapeMap
import es.weso.shapemaps.Info
import es.weso.shex.validator.ExternalIRIResolver
import es.weso.shex.validator.Validator
import es.weso.shapemaps.ShapeMap
import es.weso.rdf.PrefixMap
import es.weso.utils.IOUtils._

abstract trait Entry {
 def node: RDFNode
 def entryType: IRI
 def status: Status
 def name: String
 def toTestEntry(uri: URI): TestEntry
}
  
case class RepresentationTest(
  override val node: RDFNode,
  override val status: Status,
  override val name: String,
  json: IRI,
  shex: IRI,
  ttl: IRI) extends Entry {
    override val entryType = sht_RepresentationTest

    override def toTestEntry(shexFolderURI: URI): TestEntry = {
      val id = TestId(name)
      TestEntry(
       name = id, 
      action = {
        val resolvedJson = mkLocal(json,schemasBase,shexFolderURI)
        val resolvedShEx = mkLocal(shex,schemasBase,shexFolderURI)
                  // info(s"Entry: $r with json: ${resolvedJsonIri}")
        val res: IO[TestResult] = for {
          jsonStr <- derefUriIO(resolvedJson)
          schemaStr <- derefUriIO(resolvedShEx)
          schema <- Schema.fromString(schemaStr, "SHEXC", None)
          expectedSchema <- jsonStr2Schema(jsonStr)
          r <- if (CompareSchemas.compareSchemas(schema, expectedSchema)) {
            parse(jsonStr) match {
             case Left(err) => IO.pure(
               FailedResult(id,
                msg = s"Schemas are equal but error parsing Json $jsonStr")
             )
             case Right(json) => {
             if (json.equals(schema2Json(schema))) {
              IO.pure(
                PassedResult(id,msg = s"Schemas are equal")
              )
             } else {
              IO.pure(FailedResult(id, 
               msg = Some(s"Json's are different\nSchema:${schema}\nJson generated: ${schema2Json(schema).spaces2}\nExpected: ${json.spaces2}")
              ))
             }
            }
           } } else {
            IO.pure(
              FailedResult(id, 
               msg = Some(s"Schemas are different. Parsed:\n${schema}\n-----Expected:\n${expectedSchema}")
              ))
           }
          } yield r
         res  
      }
    )
    }
  }
  
case class Validate(override val node: RDFNode,
                      override val status: Status,
                      override val name: String,
                      action: ManifestAction,
                      result: Result,
                      specRef: Option[IRI]
                     ) extends Entry {
    override val entryType = sht_Validate
    override def toTestEntry(uri: URI): TestEntry = {
      val id = TestId(name),
      TestEntry(
        name = id, 
        action = IO.pure(PassedResult(id, msg="Not implemented"))
      )
    }
 }
  
 abstract class ValidOrFailureTest(override val node: RDFNode,
                                override val status: Status,
                                override val name: String,
                                val traits: List[IRI],
                                val comment: String,
                                val action: Action,
                                val maybeResult: Option[IRI],
                                val entryType : IRI
                                ) extends Entry {
    override def toTestEntry(uri: URI): TestEntry = 
      TestEntry(
        name = TestId(name), 
        action = { 
          val base = Paths.get(".").toUri
          action match {
            case focusAction: FocusAction => validateFocusAction(focusAction,base,this,true, uri)
            case mr: MapResultAction => validateMapResult(mr,base,this,uri)
            case ma: ManifestAction => err(s"Not implemented validate ManifestAction yet")
          }
        }
      )
  }
  
  case class ValidationTest(override val node: RDFNode,
                      override val status: Status,
                      override val name: String,
                      override val traits: List[IRI],
                      override val comment: String,
                      override val action: Action,
                      override val maybeResult: Option[IRI]
                     ) extends
    ValidOrFailureTest(node,status,name,traits,comment,action,maybeResult, sht_ValidationTest) {
  }
  
  case class ValidationFailure(override val node: RDFNode,
                               override val status: Status,
                               override val name: String,
                               override val traits: List[IRI],
                               override val comment: String,
                               override val action: Action,
                               override val maybeResult: Option[IRI]
                              ) extends
    ValidOrFailureTest(node,status,name,traits,comment,action,maybeResult,sht_Validate) {
  }
  
  case class NegativeSyntax(override val node: RDFNode,
                            override val status: Status,
                            override val name: String,
                            shex: IRI
                           ) extends Entry {
    override val entryType = sht_NegativeSyntax
    override def toTestEntry(uri: URI): TestEntry = 
      TestEntry(
        name = TestId(name), 
        action = IO.println(s"Pending") *> IO(true)
      )
  }
  
  case class NegativeStructure(override val node: RDFNode,
                            override val status: Status,
                            override val name: String,
                            shex: IRI
                           ) extends Entry {
    override val entryType = sht_NegativeStructure
    override def toTestEntry(uri: URI): TestEntry = 
      TestEntry(
        name = TestId(name), 
        action = IO.println(s"Pending") *> IO(true)
      )


}
  