package es.weso.shex

import cats._
import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
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
 
 override def getShape(sl: ShapeLabel): Either[String, ShapeExpr] = ???

 override def getTripleExpr(sl: ShapeLabel): Either[String, TripleExpr] = ???
}

object ResolvedSchema {

 private case class MapsImported(
  shapeExprMaps: Map[ShapeLabel,(ShapeExpr,Option[IRI])], 
  tripleExprMaps: Map[ShapeLabel,(TripleExpr,Option[IRI])]
) {
 def merge(schema: Schema, iri: IRI): MapsImported = {
   this.copy(
      shapeExprMaps = cnvMap(schema.shapesMap, addIri[ShapeExpr](iri)) ++ shapeExprMaps,
      tripleExprMaps = cnvMap(schema.tripleExprMap, addIri[TripleExpr](iri)) ++ tripleExprMaps
     ) 
  } 
 }

 // TODO: I think this can be easier with cats instances but I am not sure now how to invoke it
 private def cnvMap[A,K,B](m: Map[K,A], f: A => B): Map[K,B] = m.map { case(k,a) => (k,f(a)) } 

 private def addNone[A,B](v: A): (A, Option[B]) = (v,None)
 private def addIri[A](iri: IRI)(v: A): (A,Option[IRI]) = (v,Some(iri))

 /**
    * Resolves import declarations in schema
    * @param schema
    * @return a resolved schema
    */
  def resolve(schema: Schema, base: Option[IRI]): IO[ResolvedSchema] = for {
    mapsImported <- closureImports(
      schema.imports, 
      List(schema.id), 
      MapsImported(
        cnvMap(schema.shapesMap, addNone[ShapeExpr,IRI]),
        cnvMap(schema.tripleExprMap,addNone[TripleExpr,IRI])
        ),
      base  
     )
  } yield ResolvedSchema(
    source = schema, 
    resolvedMapShapeExprs = mapsImported.shapeExprMaps,
    resolvedMapTripleExprs = mapsImported.tripleExprMaps
  )



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
      sm <- closureImports(is ++ schema.imports, i :: visited, current.merge(schema,i),base)
    } yield sm
  }


}
