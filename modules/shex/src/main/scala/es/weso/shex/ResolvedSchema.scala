package es.weso.shex

import cats.implicits._
import es.weso.rdf.nodes._
import scala.util._
import cats.effect.IO
import es.weso.depgraphs.Inheritance
import es.weso.depgraphs.InheritanceJGraphT
import es.weso.rdf.locations.Location
import es.weso.utils.VerboseLevel

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
  inheritanceGraph: Inheritance[ShapeLabel, ShapesRelation],
  labelLocationMap: Option[Map[ShapeLabel,Location]]
  ) extends AbstractSchema {
  
 def id = source.id
 def prefixes = source.prefixes
 def base = source.base
 def startActs = source.startActs
 def start = source.start

 override def labels: List[ShapeLabel] = (resolvedMapShapeExprs.keySet ++ resolvedMapTripleExprs.keySet).toList

 override def shapes = {
   val se = resolvedMapShapeExprs.toList.map { case (_,rse) => rse.se }
   if (se.isEmpty) None 
   else Some(se)
 }

 def maybeTripleExprMap = source.tripleExprMap
 def imports = source.imports
 
 override def getShape(sl: ShapeLabel): Either[String, ShapeExpr] = {
  resolvedMapShapeExprs.get(sl).toRight(s"Not found $sl").map(_.se) 
 }

 def isNonAbstract(sl: ShapeLabel): Boolean = {
   getShape(sl).fold(_ => false, se => se match {
     case _: ShapeDecl => false 
     case _ => true
   })
 }


 override def getTripleExpr(sl: ShapeLabel): Either[String, TripleExpr] = 
  resolvedMapTripleExprs.get(sl).toRight(s"Not found $sl").map(_.te) 

 // override def addShape(se: ShapeExpr): es.weso.shex.Schema = ???
 // override def labels: List[ShapeLabel] = ???
 lazy val optTripleExprMap: Option[Map[ShapeLabel,TripleExpr]] = Some(resolvedMapTripleExprs.mapValues(_.te).toMap)

}

object ResolvedSchema {

 private case class MapsImported(
  shapeExprMaps: Map[ShapeLabel,ResolvedShapeExpr],
  tripleExprMaps: Map[ShapeLabel,ResolvedTripleExpr]
) {
 def merge(schema: Schema, iri: IRI): MapsImported = {
   
   this.copy(
      shapeExprMaps = schema.shapesMap.mapValues(ResolvedShapeExpr(_,iri)).toMap ++ shapeExprMaps,
      tripleExprMaps = schema.tripleExprMap.mapValues(ResolvedTripleExpr(_,iri)).toMap ++ tripleExprMaps
     ) 
  } 
 }

 /**
    * Resolves import declarations in schema
    * @param schema
    * @return a resolved schema
    */
  def resolve(schema: Schema, base: Option[IRI], verboseLevel: VerboseLevel): IO[ResolvedSchema] =
   for {
     mapsImported <- closureImports(schema.imports,
      List(schema.id), 
      MapsImported(
        schema.shapesMap.mapValues(ResolvedShapeExpr(_)).toMap,
        schema.tripleExprMap.mapValues(ResolvedTripleExpr(_)).toMap
        ),
      base, 
      verboseLevel)
     inheritanceGraph <- InheritanceGraph.mkInheritanceGraph(mapsImported.shapeExprMaps, verboseLevel)
   } yield ResolvedSchema(
    source = schema, 
    resolvedMapShapeExprs = mapsImported.shapeExprMaps.toMap,
    resolvedMapTripleExprs = mapsImported.tripleExprMaps.toMap,
    inheritanceGraph,
    labelLocationMap = schema.labelLocationMap
  )

  // TODO: make the following method tailrecursive
  private def closureImports(imports: List[IRI],
                             visited: List[IRI],
                             current: MapsImported,
                             base: Option[IRI],
                             verbose: VerboseLevel
                            ): IO[MapsImported] = imports match {
    case Nil => IO.pure(current)
    case (i::is) => 
      if (visited contains i) 
        closureImports(is,visited,current,base,verbose)
      else Schema
         .fromIRI(i,base, verbose)
         .flatMap(schema => 
           closureImports(is ++ schema.imports, i :: visited, current.merge(schema,i),base, verbose)
         )
  }

  def empty: IO[ResolvedSchema] = 
   InheritanceGraph.empty.map(ig => 
    ResolvedSchema(
     source = Schema.empty, 
     resolvedMapShapeExprs = Map(),
     resolvedMapTripleExprs = Map(),
     inheritanceGraph = ig,
     labelLocationMap = None
    ))

}

