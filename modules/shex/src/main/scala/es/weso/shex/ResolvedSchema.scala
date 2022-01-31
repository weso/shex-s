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
  inheritanceGraph: Inheritance[ShapeLabel],
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
   // pprint.log(sl,tag="getShape")
   // pprint.log(resolvedMapShapeExprs,tag="ResolvedMapShapeExprs")
  resolvedMapShapeExprs.get(sl).toRight(s"Not found $sl").map(_.se) 
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
     inheritanceGraph <- mkInheritanceGraph(mapsImported.shapeExprMaps, verboseLevel)
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
    case (i::is) => if (visited contains i) closureImports(is,visited,current,base,verbose)
    else for {
      schema <- Schema.fromIRI(i,base, verbose)
      sm <- closureImports(is ++ schema.imports, i :: visited, current.merge(schema,i),base, verbose)
    } yield sm
  }

  private def addLs(g: Inheritance[ShapeLabel],
                    ls: List[ShapeLabel], 
                    sub: ShapeLabel) = {
    def cmb(c: Unit, e: ShapeLabel): IO[Unit] = 
      g.addInheritance(sub,e)
    ls.foldM(())(cmb)
  }
  

  private def addExtendsRestricts(g: Inheritance[ShapeLabel],
                         sub: ShapeLabel,
                         shape: Shape
                         ): IO[Unit] = {
    (shape._extends,shape.restricts) match {
      case (None,None) => ().pure[IO]
      case (Some(es),None) => addLs(g,es,sub) 
      case (None,Some(rs)) => addLs(g,rs,sub)
      case (Some(es),Some(rs)) => addLs(g,es ++ rs, sub)
   }
  }

  // TODO: Check possible infinite loop when shape exprs contain themselves...
   private def addShapeExpr(g: Inheritance[ShapeLabel], 
                sub: ShapeLabel, 
                se: ShapeExpr,
                verbose: VerboseLevel
                ): IO[Unit] = {
    se match {
      case s: Shape => addExtendsRestricts(g,sub, s) 
      case s: ShapeAnd => {
         def f(x: Unit, se: ShapeExpr): IO[Unit] = {
           // verbose.debug(s"Inside and ${sub.toRDFNode.show}: new shape: ${se.id.map(_.toRDFNode.show).getOrElse("?")}") *>
           // TODO: Check visited before?
           addShapeExpr(g, sub, se, verbose)
         }
         // verbose.debug(s"ShapeAnd: ${sub.toRDFNode.show}: $s") *>  
         s.shapeExprs.foldM(())(f)
      }
      case ShapeDecl(l,_,se) => se match {
        case _ => addShapeExpr(g,sub, se, verbose)  
      }
      case _ => ().pure[IO]
     }
   }
  
  private def addPair(
    g: Inheritance[ShapeLabel], 
    verboseLevel: VerboseLevel)(
    u: Unit, 
    pair: (ShapeLabel,ResolvedShapeExpr)
    ): IO[Unit] = {
     val (shapeLabel,resolvedShapeExpr) = pair
     addShapeExpr(g, shapeLabel, resolvedShapeExpr.se, verboseLevel)
   }

  private def mkInheritanceGraph(
    m: Map[ShapeLabel,ResolvedShapeExpr],
    verboseLevel: VerboseLevel
  ): IO[Inheritance[ShapeLabel]] = for {
    g <- InheritanceJGraphT.empty[ShapeLabel]
    _ <- m.toList.foldM(())(addPair(g, verboseLevel))
   } yield g

  def empty: IO[ResolvedSchema] = for {
    ig <- InheritanceJGraphT.empty[ShapeLabel]
  } yield ResolvedSchema(
    source = Schema.empty, 
    resolvedMapShapeExprs = Map(),
    resolvedMapTripleExprs = Map(),
    inheritanceGraph = ig,
    labelLocationMap = None
  )

}

