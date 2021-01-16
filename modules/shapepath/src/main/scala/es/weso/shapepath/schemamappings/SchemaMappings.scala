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


case class SchemaMapping(source: ShapePath, target: ShapePath) {

} 

object SchemaMapping {
  def fromString(str: String, line: Option[Int] = None): Either[ParseError, SchemaMapping] = {
    ???
  }
}

case class SchemaMappings(
  prefixMap: PrefixMap,
  mappings: List[SchemaMapping]
  ) {
    def convert(schema: Schema): Either[ConversionError,Schema] = ???
}

object SchemaMappings {
  def fromString(str: String, base: Option[IRI]): Either[ParseError, SchemaMappings] = { 
     // str.split("\n").map(SchemaMapping.fromString(_)).toList.sequence.map(SchemaMappings(_))
     import SchemaMappingsParser._
     parseSchemaMappings(str.toString, base).leftMap(str => ParseError(str, None))
  }

  def empty: SchemaMappings = 
   SchemaMappings(
     prefixMap = PrefixMap.empty,
     mappings = List()
   )

}