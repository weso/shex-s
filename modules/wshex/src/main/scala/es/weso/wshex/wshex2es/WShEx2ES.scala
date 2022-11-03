package es.weso.wshex.wshex2es

import cats._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso._
import es.weso.rbe.interval.{IntLimit, Unbounded}
import es.weso.rdf.nodes._
import es.weso.wbmodel.{Property => _, _}
import es.weso.rbe.interval.IntOrUnbounded
import scala.collection.compat._ // Required for partitionMap
import es.weso.rdf.nodes._
import es.weso.wshex._
import es.weso.wshex.TermConstraint._
import es.weso._
import es.weso.rdf.PrefixMap

case class WShEx2ES(convertOptions: WShEx2ESConvertOptions) extends LazyLogging {

  type Convert[A] = Either[WShEx2ESConvertError, A]

  /** Convert an WShEx schema to an Entity Schema in ShEx
    */
  def convert(
      wschema: WSchema
  ): Convert[shex.AbstractSchema] = for {
    shapes <- convertShapesMap(wschema.shapesMap)
    pm <- convertPrefixMap(wschema.prefixes)
  } yield shex.Schema.empty.withPrefixMap(pm).withShapes(shapes)

  private def convertShapesMap(
      m: Map[ShapeLabel, WShapeExpr]
  ): Convert[Option[List[shex.ShapeExpr]]] =
    if (m.isEmpty) none.asRight
    else
      m.toList
        .map { case (lbl, se) =>
          convertLabeledShapeExpr(lbl, se)
        }
        .sequence
        .map(_.some)

  private def convertPrefixMap(prefixes: Option[PrefixMap]): Convert[Option[PrefixMap]] =
    ok(
      PrefixMap
        .fromMap(
          Map(
            convertOptions.entityAlias -> convertOptions.entityIri,
            convertOptions.directPropertyAlias -> convertOptions.directPropertyIri,
            convertOptions.propAlias -> convertOptions.propIri,
            convertOptions.propStatementAlias -> convertOptions.propStatementIri,
            convertOptions.propQualifierAlias -> convertOptions.propQualifierIri
          )
        )
        .some
    )

  private def convertLabeledShapeExpr(lbl: ShapeLabel, se: WShapeExpr): Convert[shex.ShapeExpr] =
    convertShapeLabel(lbl).flatMap(l => convertShapeExpr(se).map(_.addId(l)))

  private def convertShapeExpr(se: WShapeExpr): Convert[shex.ShapeExpr] = se match {
    case wnc: WNodeConstraint => convertNodeConstraint(wnc)
    case s: WShape            => convertShape(s)
    case sa: WShapeAnd =>
      for {
        l <- convertId(sa.id)
        ls <- sa.exprs.map(convertShapeExpr).sequence
      } yield shex.ShapeAnd(id = l, shapeExprs = ls, annotations = None, actions = None)
    case so: WShapeOr =>
      for {
        l <- convertId(so.id)
        ls <- so.exprs.map(convertShapeExpr).sequence
      } yield shex.ShapeOr(id = l, shapeExprs = ls, annotations = None, actions = None)
    case sn: WShapeNot =>
      for {
        l <- convertId(sn.id)
        se <- convertShapeExpr(sn.shapeExpr)
      } yield shex.ShapeNot(id = l, shapeExpr = se, annotations = None, actions = None)
    case sr: WShapeRef =>
      for {
        ref <- convertShapeLabel(sr.label)
      } yield shex.ShapeRef(reference = ref, annotations = None, actions = None)
  }

  private def convertShapeLabel(lbl: ShapeLabel): Convert[shex.ShapeLabel] = lbl match {
    case Start             => shex.Start.asRight
    case iriLbl: IRILabel  => shex.IRILabel(iriLbl.iri).asRight
    case bnLbl: BNodeLabel => shex.BNodeLabel(bnLbl.bnode).asRight
  }

  private def convertId(id: Option[ShapeLabel]): Convert[Option[shex.ShapeLabel]] =
    convertOpt(id, convertShapeLabel)

  private def convertNodeConstraint(nc: WNodeConstraint): Convert[shex.NodeConstraint] =
    for {
      id <- convertId(nc.id)
      nk <- convertOpt(nc.kind, convertKind)
      dt <- convertOpt(nc.datatype, convertDatatype)
      facets <- convertFacets(nc.xsFacets)
      values <- convertOpt(nc.values, convertValues)
    } yield shex.NodeConstraint(
      id = id,
      nodeKind = nk,
      datatype = dt,
      xsFacets = facets,
      values = values,
      annotations = None,
      actions = None
    )

  private def convertKind(kind: WNodeKind): Convert[shex.NodeKind] = kind match {
    case WNodeKind.LiteralKind => ok(shex.LiteralKind)
    case _                     => err(NotImplementedNodeKind(kind))
  }

  private def convertDatatype(dt: IRI): Convert[IRI] = ok(dt)

  private def convertFacets(facets: List[shex.XsFacet]): Convert[List[shex.XsFacet]] =
    ok(facets)

  private def convertValues(vs: List[ValueSetValue]): Convert[List[shex.ValueSetValue]] =
    vs.map(convertValue(_)).sequence

  private def convertValue(v: ValueSetValue): Convert[shex.ValueSetValue] = v match {
    case eid: EntityIdValueSetValue => ok(shex.IRIValue(eid.id.iri))
    case iv: IRIValueSetValue       => ok(shex.IRIValue(iv.iri))
    case sv: StringValueSetValue    => ok(shex.StringValue(sv.str))
    case is: IRIStem                => ok(shex.IRIStem(is.stem))
  }

  private def convertShape(s: WShape): Convert[shex.ShapeExpr] = for {
    id <- convertId(s.id)
    te <- convertOpt(s.expression, convertTripleExpr)
    extras <- s.extras.map(convertExtra(_)).sequence
  } yield {
    val extra = if (extras.isEmpty) none else extras.some
    id match {
      case None      => shex.Shape.empty.withExpr(te).withExtra(extra)
      case Some(lbl) => shex.ShapeDecl(lbl, shex.Shape.empty.withExpr(te).withExtra(extra))
    }
  }

  private def convertTripleExpr(te: TripleExpr): Convert[shex.TripleExpr] = te match {
    case eo: EachOf =>
      for {
        id <- convertId(eo.id)
        es <- convertList(eo.exprs, convertTripleExpr)
        min <- convertOpt(eo.optMin, convertMin)
        max <- convertOpt(eo.optMax, convertMax)
      } yield shex.EachOf(
        id = id,
        expressions = es,
        optMin = min,
        optMax = max,
        semActs = None,
        annotations = None
      )
    case oo: OneOf =>
      for {
        id <- convertId(oo.id)
        es <- convertList(oo.exprs, convertTripleExpr)
        min <- convertOpt(oo.optMin, convertMin)
        max <- convertOpt(oo.optMax, convertMax)
      } yield shex.OneOf(
        id = id,
        expressions = es,
        optMin = min,
        optMax = max,
        semActs = None,
        annotations = None
      )
    case EmptyTripleExpr      => ???
    case tc: TripleConstraint => convertTripleConstraint(tc)
  }

  private def convertExtra(e: PropertyId): Convert[IRI] =
    ok(e.iri)

  private def ok[A](x: A): Convert[A] = x.asRight[WShEx2ESConvertError]

  private def err[A](e: WShEx2ESConvertError): Convert[A] =
    e.asLeft[A]

  private def convertOpt[A, B](x: Option[A], cnv: A => Convert[B]): Convert[Option[B]] =
    x.fold(ok(none[B]))(cnv(_).map(_.some))

  private def convertList[A, B](xs: List[A], cnv: A => Convert[B]): Convert[List[B]] =
    xs.map(cnv(_)).sequence

  private def convertMin(min: Int): Convert[Int] =
    ok(min)

  private def convertMax(max: Max): Convert[shex.Max] = max match {
    case mint: IntLimit => ok(shex.IntMax(mint.m))
    case Unbounded      => ok(shex.Star)
  }

  private def convertTripleConstraint(tc: TripleConstraint): Convert[shex.TripleExpr] = tc match {
    case tcl: TripleConstraintLocal =>
      for {
        pred <- convertProperty(tcl.property)
        value <-
          if (tcl.value == WNodeConstraint.emptyExpr) ok(none)
          else convertNodeConstraint(tcl.value).map(_.some)
        max <- convertMax(tcl.max)
        // TODO: Convert qualifiers...
      } yield shex.TripleConstraint(
        id = None,
        optInverse = None,
        optNegated = None,
        predicate = pred,
        valueExpr = value,
        optMin = tcl.min.some,
        optMax = max.some,
        optVariableDecl = None,
        semActs = None,
        annotations = None
      )
    case tcr: TripleConstraintRef =>
      for {
        pred <- convertProperty(tcr.property)
        value <- convertShapeExpr(tcr.value)
        max <- convertMax(tcr.max)
        // TODO: Convert qualifiers...
      } yield shex.TripleConstraint(
        id = None,
        optInverse = None,
        optNegated = None,
        predicate = pred,
        valueExpr = value.some,
        optMin = tcr.min.some,
        optMax = max.some,
        optVariableDecl = None,
        semActs = None,
        annotations = None
      )
    case tcg: TripleConstraintGeneral =>
      for {
        pred <- convertProperty(tcg.property)
        value <- convertShapeExpr(tcg.value)
        max <- convertMax(tcg.max)
        // TODO: Convert qualifiers...
      } yield shex.TripleConstraint(
        id = None,
        optInverse = None,
        optNegated = None,
        predicate = pred,
        valueExpr = value.some,
        optMin = tcg.min.some,
        optMax = max.some,
        optVariableDecl = None,
        semActs = None,
        annotations = None
      )
  }

  private def convertProperty(prop: PropertyId): Convert[IRI] =
    ok(convertOptions.directPropertyIri + prop.id)

}

object WShEx2ES {
  def apply(
      convertOptions: WShEx2ESConvertOptions = WShEx2ESConvertOptions.default
  ): WShEx2ES =
    // Note: 'new' is needed to avoid infinite loop
    new WShEx2ES(convertOptions)

}
