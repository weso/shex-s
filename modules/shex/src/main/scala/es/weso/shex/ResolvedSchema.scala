package es.weso.shex

import cats.implicits._
import es.weso.rdf.nodes._
import scala.util._
import cats.effect.IO
import es.weso.depgraphs.Inheritance
import es.weso.depgraphs.InheritanceJGraphT
import es.weso.rdf.locations.Location
import es.weso.utils.VerboseLevel
import java.nio.file.{Path => FilePath}
import es.weso.utils.internal.CollectionCompat._

/** Represents a schema with all the imports resolved
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
    labelLocationMap: Option[Map[ShapeLabel, Location]]
) extends AbstractSchema {

  def id = source.id
  def prefixes = source.prefixes
  def base = source.base
  def startActs = source.startActs
  def start = source.start

  override def labels: List[ShapeLabel] =
    (resolvedMapShapeExprs.keySet ++ resolvedMapTripleExprs.keySet).toList

  override def shapes = {
    val se = resolvedMapShapeExprs.toList.map { case (_, rse) => rse.se }
    if (se.isEmpty) None
    else Some(se)
  }

  def maybeTripleExprMap = source.tripleExprMap
  def imports = source.imports

  override def getShape(sl: ShapeLabel): Either[String, ShapeExpr] =
    resolvedMapShapeExprs.get(sl).toRight(s"Not found $sl").map(_.se)

  def isNonAbstract(sl: ShapeLabel): Boolean = getShape(sl).fold(
    _ => false,
    se =>
      se match {
        case sd: ShapeDecl => !sd._abstract
        case _             => true
      }
  )

  override def getTripleExpr(sl: ShapeLabel): Either[String, TripleExpr] =
    resolvedMapTripleExprs.get(sl).toRight(s"Not found $sl").map(_.te)

  lazy val optTripleExprMap: Option[Map[ShapeLabel, TripleExpr]] =
    mapValues[ShapeLabel, ResolvedTripleExpr, TripleExpr](resolvedMapTripleExprs)(_.te).some
}

object ResolvedSchema {

  private case class MapsImported(
      shapeExprMaps: Map[ShapeLabel, ResolvedShapeExpr],
      tripleExprMaps: Map[ShapeLabel, ResolvedTripleExpr]
  ) {
    def merge(schema: Schema, iri: IRI): MapsImported =
      this.copy(
        shapeExprMaps = mapValues(schema.shapesMap)(ResolvedShapeExpr(_, iri)) ++ shapeExprMaps,
        tripleExprMaps =
          mapValues(schema.tripleExprMap)(ResolvedTripleExpr(_, iri)) ++ tripleExprMaps
      )
  }

  /** Resolves import declarations in schema
    * @param schema
    * @return a resolved schema
    */
  def resolve(
      schema: Schema,
      base: Option[IRI] = None,
      verboseLevel: VerboseLevel = VerboseLevel.Nothing,
      assumeLocal: Option[(IRI, FilePath)] = None
  ): IO[ResolvedSchema] =
    for {
      mapsImported <- closureImports(
        schema.imports,
        List(schema.id),
        MapsImported(
          mapValues(schema.shapesMap)(ResolvedShapeExpr(_)),
          mapValues(schema.tripleExprMap)(ResolvedTripleExpr(_))
        ),
        base,
        verboseLevel,
        assumeLocal
      )
      inheritanceGraph <- InheritanceGraph.mkInheritanceGraph(
        mapsImported.shapeExprMaps,
        verboseLevel
      )
      shapeExprMapsWithDescendants <- addDescendantsInfo(
        inheritanceGraph,
        mapsImported.shapeExprMaps
      )
    } yield ResolvedSchema(
      source = schema,
      resolvedMapShapeExprs = shapeExprMapsWithDescendants,
      resolvedMapTripleExprs = mapsImported.tripleExprMaps,
      inheritanceGraph,
      labelLocationMap = schema.labelLocationMap
    )

  private def addDescendantsInfo(
      inheritanceGraph: Inheritance[ShapeLabel, ShapesRelation],
      seMap: Map[ShapeLabel, ResolvedShapeExpr]
  ): IO[Map[ShapeLabel, ResolvedShapeExpr]] =
    cnvMapIO(seMap, addDescendantInfoSE(inheritanceGraph, _))

  private def cnvMapIO[K, A, B](m: Map[K, A], f: A => IO[B]): IO[Map[K, B]] =
    mapValues(m)(f).toList.map { case (k, ioa) => ioa.map(v => (k, v)) }.sequence.map(_.toMap)

  private def addDescendantInfoSE(
      inheritance: Inheritance[ShapeLabel, ShapesRelation],
      rse: ResolvedShapeExpr
  ): IO[ResolvedShapeExpr] =
    getDescendants(inheritance, rse.se)
      .map(rse.withDescendants(_))

  private def getDescendants(
      inheritance: Inheritance[ShapeLabel, ShapesRelation],
      se: ShapeExpr
  ): IO[Set[ShapeLabel]] = se.id match {
    case None      => IO.pure(Set[ShapeLabel]())
    case Some(lbl) => inheritance.descendantsByEdgtypes(lbl, Set(Extends, References))
  }

  private def getEffectiveIRI(iri: IRI, assumeLocal: Option[(IRI, FilePath)]): IRI =
    assumeLocal match {
      case Some((prefix, path)) if iri.str.startsWith(prefix.str) =>
        val local = iri.str.stripPrefix(prefix.str)
        val newIri = IRI(path.resolve(local).toUri)
        newIri
      case _ => iri
    }

  // TODO: make the following method tailrecursive
  private def closureImports(
      imports: List[IRI],
      visited: List[IRI],
      current: MapsImported,
      base: Option[IRI],
      verbose: VerboseLevel,
      assumeLocal: Option[(IRI, FilePath)]
  ): IO[MapsImported] = imports match {
    case Nil => IO.pure(current)
    case (i :: is) =>
      if (visited contains i) closureImports(is, visited, current, base, verbose, assumeLocal)
      else {
        val effectiveIRI = getEffectiveIRI(i, assumeLocal)
        verbose.debug(s"Resolving schema import: i=${i}") *>
          verbose.debug(s"Resolving schema import: assumeLocal=${assumeLocal}") *>
          verbose.debug(s"Resolving schema import: effectiveIRI=${effectiveIRI}") *>
          Schema
            .fromIRI(effectiveIRI, base, verbose, assumeLocal)
            .flatMap(schema =>
              closureImports(
                is ++ schema.imports,
                i :: visited,
                current.merge(schema, i),
                base,
                verbose,
                assumeLocal
              )
            )
      }
  }

  def empty: IO[ResolvedSchema] =
    InheritanceGraph.empty.map(ig =>
      ResolvedSchema(
        source = Schema.empty,
        resolvedMapShapeExprs = Map(),
        resolvedMapTripleExprs = Map(),
        inheritanceGraph = ig,
        labelLocationMap = None
      )
    )

}
