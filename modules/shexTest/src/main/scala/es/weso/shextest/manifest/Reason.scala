package es.weso.shextest.manifest

import io.circe.Json
import es.weso.shapemaps.ResultShapeMap
import es.weso.rdf.nodes.RDFNode
import es.weso.shapemaps.ShapeMapLabel
import es.weso.shex.Schema

sealed abstract class Reason 

object Reason {
 
 final case class Ignored(name: String) extends Reason
 final case class NotImplemented(msg: String) extends Reason
 final case class JsonResultsMatch(json: JsonResult) extends Reason
 final case class JsonResultsDifferent(obtained: ResultShapeMap, expected: JsonResult) extends Reason
 final case class ConformantMatch(focus: RDFNode, lbl: ShapeMapLabel, result: ResultShapeMap) extends Reason 
 final case class ConformsButShoudnt(focus: RDFNode, lbl: ShapeMapLabel, dataStr: String, schemaStr: String, resultShapeMap: ResultShapeMap) extends Reason {
     override def toString = s"""|Focus $focus conforms to $lbl but should not
                                 |Data: \n${dataStr}
                                 |Schema: ${schemaStr}
                                 |${resultShapeMap.getInfo(focus, lbl)}""".stripMargin
 }
 final case class DoesntConformButShould(focus: RDFNode, lbl: ShapeMapLabel, dataStr: String, schemaStr: String, resultShapeMap: ResultShapeMap) extends Reason {
     override def toString = s"""|Focus $focus does not conform to $lbl but should
                                  |Data: \n${dataStr}
                                  |Schema: ${schemaStr}
                                  |${resultShapeMap.getInfo(focus, lbl)}
                                  |""".stripMargin
 }
 final case object DoesntValidateAsExpected extends Reason
 final case class UnsupportedEntryType(et: Entry) extends Reason
 final case class ErrorParsingJsonStr(jsonStr: String) extends Reason
 final case object JsonsEqual extends Reason
 final case class JsonsDifferent(schema: Schema, jsonGenerated: Json, expected: Json) extends Reason
 final case class SchemasDifferent(schema: Schema, expected: Schema) extends Reason
 final case class ParsedOKWithNegativeSyntax(schema: Schema, schemaStr: String) extends Reason
 final case class ParsedFailedAsExpected(msg: String) extends Reason
 final case class SchemaParsedWithNegativeStructure(schema: Schema, schemaStr: String) extends Reason
 final case object SchemaWellFormed extends Reason
}
