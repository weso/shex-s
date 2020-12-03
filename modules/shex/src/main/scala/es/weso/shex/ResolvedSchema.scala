package es.weso.shex

// import cats._
import cats.implicits._
//import es.weso.rdf._
import es.weso.rdf.nodes._
import scala.util._
import cats.effect.IO
import es.weso.depgraphs.Inheritance
import es.weso.depgraphs.InheritanceJGraphT

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
  inheritanceGraph: Inheritance[ShapeLabel]
  ) extends AbstractSchema {
  
 def id = source.id
 def prefixes = source.prefixes
 def base = source.base
 def startActs = source.startActs
 def start = source.start

 override def labels: List[ShapeLabel] = (resolvedMapShapeExprs.keySet ++ resolvedMapTripleExprs.keySet).toList

 def shapes = source.shapes
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
  def resolve(schema: Schema, base: Option[IRI]): IO[ResolvedSchema] =
   for {
     mapsImported <- closureImports(schema.imports,
      List(schema.id), 
      MapsImported(
        cnvMap(schema.shapesMap, (v: ShapeExpr) => ResolvedShapeExpr(v)),
        cnvMap(schema.tripleExprMap, (v: TripleExpr) => ResolvedTripleExpr(v))
        ),
      base)
     inheritanceGraph <- mkInheritanceGraph(mapsImported.shapeExprMaps)
   } yield ResolvedSchema(
    source = schema, 
    resolvedMapShapeExprs = mapsImported.shapeExprMaps,
    resolvedMapTripleExprs = mapsImported.tripleExprMaps,
    inheritanceGraph
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

  private def addExtends(g: Inheritance[ShapeLabel],
                         sub: ShapeLabel,
                         shape: Shape
                         ): IO[Unit] = 
    shape._extends match {
      case None => ().pure[IO]
      case Some(es) => {
        def cmb(c: Unit, e: ShapeLabel): IO[Unit] = 
             g.addInheritance(sub,e)
        es.foldM(())(cmb)
      }
   }

   private def addShapeExpr(g: Inheritance[ShapeLabel], 
                sub: ShapeLabel, 
                se: ShapeExpr
                ): IO[Unit] = {
    se match {
      case s: Shape => addExtends(g,sub, s) 
      case s: ShapeAnd => {
         def f(x: Unit, shape: Shape): IO[Unit] = 
           addExtends(g,sub,shape)
         s.shapeExprs.collect { case s: Shape => s}.foldM(())(f)
      }
      case ShapeDecl(l,_,se) => se match {
        case _ => addShapeExpr(g,sub, se)  
      }
      case _ => ().pure[IO]
     }
   }
  
  private def addPair(g: Inheritance[ShapeLabel])(u: Unit, pair: (ShapeLabel,ResolvedShapeExpr)): IO[Unit] = {
     val (shapeLabel,rse) = pair
     addShapeExpr(g, shapeLabel, rse.se)
   }

  private def mkInheritanceGraph(
    m: Map[ShapeLabel,ResolvedShapeExpr]
  ): IO[Inheritance[ShapeLabel]] = for {
    g <- InheritanceJGraphT.empty[ShapeLabel]
    _ <- m.toList.foldM(())(addPair(g))
   } yield g

  def empty: IO[ResolvedSchema] = for {
    ig <- InheritanceJGraphT.empty[ShapeLabel]
  } yield ResolvedSchema(
    source = Schema.empty, 
    resolvedMapShapeExprs = Map(),
    resolvedMapTripleExprs = Map(),
    inheritanceGraph = ig
  )

}

