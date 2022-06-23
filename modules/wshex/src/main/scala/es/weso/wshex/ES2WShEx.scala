package es.weso.wshex

import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso._
import es.weso.rbe.interval.{IntLimit, Unbounded}
import es.weso.rdf.nodes._
import es.weso.wbmodel._
import es.weso.wshex.esconvert._
import es.weso.rbe.interval.IntOrUnbounded

case class ESConvertOptions(
    entityIri: IRI,
    directPropertyIri: IRI,
    propIri: IRI,
    propStatementIri: IRI
)

object ESConvertOptions {
  val default = ESConvertOptions(
    entityIri = IRI("http://www.wikidata.org/entity/"),
    directPropertyIri = IRI("http://www.wikidata.org/prop/direct/"),
    propIri = IRI("http://www.wikidata.org/prop/"),
    propStatementIri = IRI("http://www.wikidata.org/prop/statement/")
  )
}

case class ES2WShEx(convertOptions: ESConvertOptions) extends LazyLogging {

  /** Convert an entity schema in ShEx to WShEx
    */
  def convertSchema(
      shexSchema: shex.AbstractSchema
  ): Either[ConvertError, Schema] =
    for {
      shapes <-
        shexSchema.shapesMap.toList.map { case (l, se) => convertLabelShapeExpr(l, se) }.sequence

      start <- shexSchema.start match {
        case None     => none.asRight
        case Some(se) => convertShapeExpr(se).flatMap(se => Right(Some(se)))
      }
    } yield Schema(shapes.toMap, start, shexSchema.prefixMap)

  private def convertLabelShapeExpr(
      label: shex.ShapeLabel,
      se: shex.ShapeExpr
  ): Either[ConvertError, (ShapeLabel, ShapeExpr)] = for {
    cse <- convertShapeExpr(se)
    lbl = convertShapeLabel(label)
  } yield (lbl, cse)

  private def convertShapeExpr(
      se: shex.ShapeExpr
  ): Either[ConvertError, ShapeExpr] =
    se match {
      case nc: shex.NodeConstraint => convertNodeConstraint(nc)
      case s: shex.Shape           => convertShape(s)
      case sand: shex.ShapeAnd =>
        for {
          ss <- sand.shapeExprs.map(convertShapeExpr(_)).sequence
        } yield ShapeAnd(id = convertId(sand.id), exprs = ss)
      case sor: shex.ShapeOr =>
        for {
          ss <- sor.shapeExprs.map(convertShapeExpr(_)).sequence
        } yield ShapeOr(id = convertId(sor.id), exprs = ss)
      case snot: shex.ShapeNot =>
        convertShapeExpr(snot.shapeExpr)
          .map(se => ShapeNot(id = convertId(snot.id), shapeExpr = se))
      case sref: shex.ShapeRef =>
        ShapeRef(convertShapeLabel(sref.reference)).asRight
      case _ => UnsupportedShapeExpr(se).asLeft
    }

  private def convertId(id: Option[shex.ShapeLabel]): Option[ShapeLabel] =
    id.map(convertShapeLabel)

  private def convertNodeConstraint(nc: shex.NodeConstraint): Either[ConvertError, NodeConstraint] =
    nc match {
      case shex.NodeConstraint(id, None, None, List(), Some(values), None, None) =>
        convertValueSet(convertId(id), values)
      // convertValueSet(values.getOrElse(List())).map(ValueSet(id, _))
      case _ => UnsupportedNodeConstraint(nc).asLeft
    }

  private def convertValueSet(
      id: Option[ShapeLabel],
      values: List[shex.ValueSetValue]
  ): Either[ConvertError, ValueSet] =
    convertValueSetValues(values)
      .map(vs => ValueSet(id, vs))

  private def convertValueSetValues(
      values: List[shex.ValueSetValue]
  ): Either[ConvertError, List[ValueSetValue]] =
    values
      .map(convertValueSetValue)
      .sequence

  private def convertValueSetValue(
      value: shex.ValueSetValue
  ): Either[ConvertError, ValueSetValue] =
    value match {
      case shex.IRIValue(i) =>
        val (name1, base1) = Utils.splitIri(i)
        logger.trace(s"""|convertValueSetValue:
              |name1: $name1
              |base1: $base1
              |""".stripMargin)
        if (IRI(base1) == convertOptions.entityIri) {
          Right(EntityIdValueSetValue(EntityId.fromIri(i)))
        } else {
          Right(IRIValueSetValue(i))
        }
      case _ => UnsupportedValueSetValue(value).asLeft
    }

  private def convertShape(s: shex.Shape): Either[ConvertError, Shape] =
    for {
      te <- optConvert(s.expression, convertTripleExpr)
    } yield Shape(
      id = convertId(s.id),
      closed = s.closed.getOrElse(false),
      extra = s.extra.getOrElse(List()).map(PropertyId.fromIRI(_)),
      expression = te
    )

  private def optConvert[A, B](
      v: Option[A],
      cnv: A => Either[ConvertError, B]
  ): Either[ConvertError, Option[B]] =
    v.fold(none[B].asRight[ConvertError])(a => cnv(a).map(Some(_)))

  private def convertTripleExpr(te: shex.TripleExpr): Either[ConvertError, TripleExpr] =
    te match {
      case eo: shex.EachOf =>
        // TODO: generalize to handle triple expressions
        for {
          tes <- eo.expressions
            .map(convertTripleExpr)
            .sequence
          tcs <- tes.map(castToTripleConstraint(_)).sequence
        } yield EachOf(tcs)
      case oo: shex.OneOf =>
        // TODO: generalize to handle triple expressions
        for {
          tes <- oo.expressions
            .map(convertTripleExpr)
            .sequence
          tcs <- tes.map(castToTripleConstraint(_)).sequence
        } yield OneOf(tcs)
      case tc: shex.TripleConstraint =>
        convertTripleConstraint(tc)
      case _ =>
        logger.warn(s"Unsupported triple expression: $te")
        Left(UnsupportedTripleExpr(te))
    }

  private def castToTripleConstraint(
      te: TripleExpr
  ): Either[ConvertError, TripleConstraint] = te match {
    case tc: TripleConstraint => Right(tc)
    case _                    => Left(CastTripleConstraintError(te))
  }

  private def makeTripleConstraint(
      pred: PropertyId,
      min: Int,
      max: IntOrUnbounded,
      se: Option[shex.ShapeExpr]
  ): Either[ConvertError, TripleConstraint] =
    se match {
      case None =>
        TripleConstraintLocal(pred, EmptyExpr, min, max).asRight
      case Some(se) =>
        convertShapeExpr(se).flatMap(s =>
          s match {
            case ShapeRef(lbl) =>
              TripleConstraintRef(pred, ShapeRef(lbl), min, max, None).asRight
            case ValueSet(id, vs) =>
              TripleConstraintLocal(pred, ValueSet(id, vs), min, max).asRight
            case _ =>
              UnsupportedShapeExpr(se).asLeft
          }
        )
    }

  private def convertTripleConstraint(
      tc: shex.TripleConstraint
  ): Either[ConvertError, TripleConstraint] = {
    val min = tc.min
    val max = tc.max match {
      case shex.Star      => Unbounded
      case shex.IntMax(m) => IntLimit(m)
    }
    val iriParsed = IRIConvert.parseIRI(tc.predicate, convertOptions)
    iriParsed match {
      case Some(DirectProperty(n)) =>
        val pred = PropertyId.fromIRI(tc.predicate)
        makeTripleConstraint(pred, min, max, tc.valueExpr)
      case Some(PropertyParsed(p)) =>
        tc.valueExpr match {
          case Some(ve) => convertTripleConstraintProperty(p, ve)
          case None     => NoValueForPropertyConstraint(p, tc).asLeft
        }
      case _ => UnsupportedPredicate(tc.predicate).asLeft
    }
  }

  private def convertTripleConstraintProperty(
      n: Int,
      t: shex.ShapeExpr
  ): Either[ConvertError, TripleConstraint] =
    t match {
      case s: shex.Shape =>
        s.expression match {
          case None => NoExprForTripleConstraintProperty(n, s).asLeft
          case Some(te) =>
            te match {
              case tc: shex.TripleConstraint =>
                val min = tc.min
                val max = tc.max match {
                  case shex.Star      => Unbounded
                  case shex.IntMax(m) => IntLimit(m)
                }
                val iriParsed = IRIConvert.parseIRI(tc.predicate, convertOptions)
                iriParsed match {
                  case Some(PropertyStatement(ns)) =>
                    if (n == ns) {
                      val pred = PropertyId.fromNumber(n, convertOptions.directPropertyIri)
                      makeTripleConstraint(pred, min, max, tc.valueExpr)
                    } else DifferentPropertyPropertyStatement(n, ns).asLeft
                  case _ => UnsupportedPredicate(tc.predicate).asLeft
                }
              case _ => UnsupportedTripleExpr(te, s"Parsing property $n").asLeft
            }
        }
      case _ => UnsupportedShapeExpr(t, s"Parsing property $n").asLeft
    }

  private def convertShapeLabel(label: shex.ShapeLabel): ShapeLabel =
    label match {
      case shex.IRILabel(iri)     => IRILabel(iri)
      case shex.BNodeLabel(bnode) => BNodeLabel(bnode)
      case shex.Start             => Start
    }

}

object ES2WShEx {
  def apply(
      convertOptions: ConvertOptions = ConvertOptions.default
  ): ShEx2WShEx =
    // Note: 'new' is needed to avoid infinite loop
    new ShEx2WShEx(convertOptions)

}
