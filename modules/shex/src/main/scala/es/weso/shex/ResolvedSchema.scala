package es.weso.shex

import cats._
import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
import scala.util._
import cats.effect.IO

/**
  * Represents a schema with all the imports resolved
  *
  * @param source
  * @param resolvedMapShapeExprs
  * @param resolvedMapTripleExprs
  */
case class ResolvedSchema(
  source: Schema,
  resolvedMapShapeExprs: Map[ShapeLabel, ResolvedShapeExpr],
  resolvedMapTripleExprs: Map[ShapeLabel, ResolvedTripleExpr],
  ) extends AbstractSchema {
  
 def id = source.id
 def prefixes = source.prefixes
 def base = source.base
 def startActs = source.startActs
 def start = source.start
 def shapes = source.shapes
 def maybeTripleExprMap = source.tripleExprMap
 def imports = source.imports
 
 override def getShape(sl: ShapeLabel): Either[String, ShapeExpr] = 
  resolvedMapShapeExprs.get(sl).toRight(s"Not found $sl").map(_.se) 

 override def getTripleExpr(sl: ShapeLabel): Either[String, TripleExpr] = 
  resolvedMapTripleExprs.get(sl).toRight(s"Not found $sl").map(_.te) 

 // override def addShape(se: ShapeExpr): es.weso.shex.Schema = ???
 // override def labels: List[ShapeLabel] = ???
 lazy val optTripleExprMap: Option[Map[ShapeLabel,TripleExpr]] = Some(resolvedMapTripleExprs.mapValues(_.te))

}

object ResolvedSchema {

 private case class MapsImported(
  shapeExprMaps: Map[ShapeLabel,ResolvedShapeExpr],
  tripleExprMaps: Map[ShapeLabel,ResolvedTripleExpr]
) {
 def merge(schema: Schema, iri: IRI): MapsImported = {
   this.copy(
      shapeExprMaps = cnvMap(schema.shapesMap, (v: ShapeExpr) => ResolvedShapeExpr(v,iri)) ++ shapeExprMaps,
      tripleExprMaps = cnvMap(schema.tripleExprMap, (v: TripleExpr) => ResolvedTripleExpr(v,iri)) ++ tripleExprMaps
     ) 
  } 
 }

 // TODO: I think this can be easier with cats instances but I am not sure now how to invoke it
 private def cnvMap[A,K,B](m: Map[K,A], f: A => B): Map[K,B] = m.map { case(k,a) => (k,f(a)) } 

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
        cnvMap(schema.shapesMap, (v: ShapeExpr) => ResolvedShapeExpr(v)),
        cnvMap(schema.tripleExprMap, (v: TripleExpr) => ResolvedTripleExpr(v))
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

  def empty: ResolvedSchema = ResolvedSchema(
    source = Schema.empty, 
    resolvedMapShapeExprs = Map(),
    resolvedMapTripleExprs = Map()
  )

}

