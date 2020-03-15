package es.weso.shex

import java.nio.file.{Files, Paths}
import cats.implicits._
import es.weso.depgraphs.DepGraph
import es.weso.rdf.{PrefixMap, RDFBuilder, RDFReader}
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.shex.shexR.{RDF2ShEx, ShEx2RDF}
import es.weso.utils.UriUtils._
import scala.io.Source
import scala.util._
import cats.effect.IO

case class ResolvedSchema(
  source: Schema,
  resolvedMapShapeExprs: Map[ShapeLabel, (ShapeExpr,Option[IRI])],
  resolvedMapTripleExprs: Map[ShapeLabel, (TripleExpr, Option[IRI])],
  ) extends AbstractSchema {
  
 def id = source.id
 def prefixes = source.prefixes
 def base = source.base
 def startActs = source.startActs
 def start = source.start
 def shapes = source.shapes
 def maybeTripleExprMap = source.tripleExprMap
 def imports = source.imports
 
}

object ResolvedSchema {

  /**
    * Resolves import declarations in schema
    * @param schema
    * @return a resolved schema
    */
  def resolve(schema: Schema): IO[ResolvedSchema] = for {
    mapsImported <- closureImports(schema.imports, List(schema.id), MapsToImport(schema.shapesMap,schema.tripleExprMap))
  } yield 

  lazy val ioMapsImported: IO[MapsToImport] = {
    closureImports(imports, List(id), MapsToImport(localShapesMap,tripleExprMap))
  }

  lazy val eitherResolvedShapesMap: IO[Map[ShapeLabel,ShapeExpr]] = {
    ioMapsImported.map(_.shapesExpr)
  }

  lazy val eitherResolvedTripleExprMap: IO[Option[Map[ShapeLabel,TripleExpr]]] = {
    ioMapsImported.map(_.maybeTripleExprs)
  }


  // TODO: make the following method tailrecursive
  private def closureImports(imports: List[IRI],
                             visited: List[IRI],
                             current: MapsImported,
                             base: Option[IRI]
                            ): IO[MapsImported] = imports match {
    case Nil => IO.pure(current)
    case (i::is) => if (visited contains i) closureImports(is,visited,current,base)
    else for {
      schema <- Schema.fromIRI(i,base)
      sm <- closureImports(is ++ schema.imports, i :: visited, current.merge(schema),base)
    } yield sm
  }

  case class MapsImported(
    shapeExprMaps: Map[ShapeLabel,(ShapeExpr,Option[IRI])], 
    tripleExprMaps: Map[ShapeLabel,(TripleExpr,Option[IRI])]
   ) {
    def merge(schema: Schema, iri: IRI): MapsImported = {
      this.copy(
        shapesExpr = schema.localShapesMap ++ shapesExpr,
        maybeTripleExprs = schema.tripleExprMap match {
          case None => maybeTripleExprs
          case Some(otherTripleExprsMap) => maybeTripleExprs match {
            case None => Some(otherTripleExprsMap)
            case Some(tripleExprsMap) => Some(otherTripleExprsMap ++ tripleExprsMap)
          }
       }
      )
    } 
  }

}
