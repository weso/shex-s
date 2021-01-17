package es.weso.shapepath.schemamappings

import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import fs2._
import fs2.io._
import es.weso.shex.Schema
import es.weso.shapepath._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI

case class ParseError(msg: String, line: Option[Int])
sealed abstract class ConversionError 
case class ErrorEvaluatingSource(source: ShapePath, schema: Schema, error: ProcessingError) extends ConversionError



case class SchemaMapping(source: ShapePath, value: IRI) {

 def convert(schema: Schema): Either[ConversionError,Schema] = {
   ShapePath.replace(source, schema, None, IRIItem(value))
   .leftMap(e => ErrorEvaluatingSource(source,schema,e))
 }

 def showQualify(pm: PrefixMap): String = 
  s"${source.showQualify(pm)} ~> ${pm.qualify(value)}"
} 

object SchemaMapping {
}

case class SchemaMappings(
  prefixMap: PrefixMap,
  mappings: List[SchemaMapping]
  ) {

  def convert(schema: Schema): Either[ConversionError,Schema] = {
   val zero = schema.asRight[ConversionError] 
   mappings
   .foldLeft(zero){ 
     case (acc, mapping) => 
      acc.flatMap(s => mapping.convert(s))
   }
   .map(_.copy(prefixes = schema.prefixes.map(_.merge(prefixMap))))
  }

  override def toString: String = 
   mappings.map(_.showQualify(prefixMap)).mkString("\n")

}

object SchemaMappings {
  def fromString(
    str: String, 
    base: Option[IRI] = None
    ): Either[ParseError, SchemaMappings] = { 
     SchemaMappingsParser.parseSchemaMappings(str.toString, base)
     .leftMap(str => ParseError(str, None))
  }

  def empty: SchemaMappings = 
   SchemaMappings(
     prefixMap = PrefixMap.empty,
     mappings = List()
   )
  

}