package es.weso.wshex

import es.weso.rbe.{Schema => _, _}
import es.weso.rdf.PREFIXES._
import es.weso.rbe.interval.IntervalChecker
import es.weso.collection.Bag
import cats._
import cats.implicits._
import es.weso.wbmodel._
import es.weso.rdf.nodes._
import es.weso.shex.XsFacet
import es.weso.shex.StringFacet
import es.weso.shex.NumericFacet
import es.weso.shex.validator.FacetChecker
import es.weso.rdf.operations.Comparisons._

sealed abstract class WShapeExpr extends Product with Serializable {

  def withLabel(label: ShapeLabel): WShapeExpr

  def dependsOn(): Set[ShapeLabel] = this match {
    case s: WShapeRef => Set(s.label)
    case s: WShape =>
      s.expression match {
        case None     => Set()
        case Some(te) => te.dependsOn()
      }
    case e: WNodeConstraint => Set()
    case sand: WShapeAnd    => sand.exprs.map(_.dependsOn()).toSet.flatten
    case sor: WShapeOr      => sor.exprs.map(_.dependsOn()).toSet.flatten
    case sn: WShapeNot      => sn.shapeExpr.dependsOn()
  }

  private def empty(): Rbe[(PropertyId, ShapeLabel)] = Empty

  def rbe: Rbe[(PropertyId, ShapeLabel)] =
    this match {
      case _: WShapeRef       => empty()
      case _: WNodeConstraint => empty()
      case s: WShape =>
        s.expression match {
          case None     => empty()
          case Some(te) =>
            // TODO: Extend with extras?
            te.rbe
        }
      case _ => empty() // TODO!!
    }

  implicit val showPair: Show[(PropertyId, ShapeLabel)] = Show.show(p => p.toString)

  private lazy val checker = IntervalChecker(rbe)

  def tripleConstraints(schema: WSchema): List[TripleConstraintRef] =
    this match {
      case sr: WShapeRef =>
        schema.get(sr.label) match {
          case None     => List()
          case Some(se) => se.tripleConstraints(schema)
        }
      case s: WShape =>
        s.expression match {
          case None => List()
          case Some(te) =>
            te match {
              case t: TripleConstraintRef   => List(t)
              case t: TripleConstraintLocal => List()
              case t: TripleConstraintGeneral => List()
              case eo: EachOf               => eo.exprs.map(_.tripleConstraints).flatten
              case oo: OneOf                => oo.exprs.map(_.tripleConstraints).flatten
              case EmptyTripleExpr          => List()
            }
        }
      case WShapeAnd(_, ls) =>
        val tcs = ls.map(_.tripleConstraints(schema)).flatten
        tcs
      case WShapeOr(_, ls) =>
        ls.map(_.tripleConstraints(schema)).flatten
      case _: WNodeConstraint => List()
      case _                  => List()
    }

  def checkNeighs(
      bag: Bag[(PropertyId, ShapeLabel)],
      failed: Set[(PropertyId, ShapeLabel)],
      schema: WSchema
  ): Either[Reason, Unit] =
    this match {
      case s: WShape =>
        checker.check(bag, s.closed) match {
          case Left(es) => Left(NoMatch(bag, rbe, es))
          case Right(_) =>
            // check that all failed properties are in Extra
            val failedPropsNotExtra = failed.filter { case (p, _) => !s.extras.contains(p) }
            if (failedPropsNotExtra.nonEmpty) {
              Left(FailedPropsNotExtra(failedPropsNotExtra))
            } else Right(())
        }
      case ns: WShapeNot =>
        ns.shapeExpr.checkNeighs(bag, failed, schema) match {
          case Left(_)  => Right(())
          case Right(_) => Left(MatchNot(bag, rbe))
        }
      case sa: WShapeAnd =>
        val es = sa.exprs.map(_.checkNeighs(bag, failed, schema))
        val errs = es.collect { case Left(err) => err }
        if (errs.nonEmpty) Left(ErrorsMatching(errs))
        else Right(())
      case so: WShapeOr =>
        // TODO: Review case where neighs pass with one shape and fail locally with another,
        // but the opposite happens and the overall result passes...
        val es = so.exprs.map(_.checkNeighs(bag, failed, schema))
        if (es.filter(_.isRight).nonEmpty) Right(())
        else Left(ShapeOr_AllFailed(es.collect { case Left(err) => err }))
      case nc: WNodeConstraint => Right(())
      case sr: WShapeRef =>
        schema.get(sr.label) match {
          case None     => Left(ShapeNotFound(sr.label, schema))
          case Some(se) => se.checkNeighs(bag, failed, schema)
        }
    }

  def checkNeighsCoded(
      bag: Bag[(PropertyId, ShapeLabel)],
      failed: Set[(PropertyId, ShapeLabel)],
      schema: WSchema
  ): Either[ReasonCode, Unit] =
    this match {
      case s: WShape =>
        checker.check(bag, s.closed) match {
          case Left(es) => Left(Reason.noMatch)
          case Right(_) =>
            // check that all failed properties are in Extra
            val failedPropsNotExtra = failed.filter { case (p, _) => !s.extras.contains(p) }
            if (failedPropsNotExtra.nonEmpty) {
              Left(Reason.failedPropsNotExtra)
            } else Right(())
        }
      case ns: WShapeNot =>
        ns.shapeExpr.checkNeighs(bag, failed, schema) match {
          case Left(_)  => Right(())
          case Right(_) => Left(Reason.matchNot)
        }
      case sa: WShapeAnd =>
        val es = sa.exprs.map(_.checkNeighs(bag, failed, schema))
        val errs = es.collect { case Left(err) => err }
        if (errs.nonEmpty) Left(Reason.errorsMatching)
        else Right(())
      case so: WShapeOr =>
        // TODO: Review case where neighs pass with one shape and fail locally with another,
        // but the opposite happens and the overall result passes...
        val es = so.exprs.map(_.checkNeighs(bag, failed, schema))
        if (es.filter(_.isRight).nonEmpty) Right(())
        else Left(Reason.shapeOr_AllFailed)
      case nc: WNodeConstraint => Right(())
      case sr: WShapeRef =>
        schema.get(sr.label) match {
          case None     => Left(Reason.shapeNotFound)
          case Some(se) => se.checkNeighsCoded(bag, failed, schema)
        }
    }

  def checkLocal(
      entity: Entity,
      fromLabel: ShapeLabel,
      schema: WSchema
  ): Either[Reason, Set[ShapeLabel]] = {
    val result: Either[Reason, Set[ShapeLabel]] = this match {
      case sr: WShapeRef =>
        schema.get(sr.label) match {
          case None => Left(ShapeNotFound(sr.label, schema))
          case Some(se) =>
            se.checkLocal(entity, fromLabel, schema)
        }
      case s: WShape =>
        s.expression match {
          case None     => Right(Set())
          case Some(te) =>
            /* println(s"""|CheckLocal................
                      |TripleExpr: $te
                      |""".stripMargin)   */
            te.checkLocal(entity, fromLabel, s.closed, s.extras)

        }
/*      case vs: ValueSet => vs.matchLocal(entity).map(_ => Set())
      case sd: StringDatatype =>
        entity match {
          //        case _: StringValue => Right(Set())
          case _ => Left(NoStringDatatype(entity))
        }
      case e: EmptyExpr => Right(Set()) */
      case nc: WNodeConstraint => nc.matchLocal(entity).map(_ => Set())
      case WShapeAnd(_, ls) =>
        val vs = ls.map(_.checkLocal(entity, fromLabel, schema)).sequence.map(_.toSet.flatten)
        vs
      case so @ WShapeOr(_, ls) =>
        // TODO: check the semantics...it may be enough if one of the values matches...
        val vs = ls.map(_.checkLocal(entity, fromLabel, schema))
        val zero: Either[Reason, Set[ShapeLabel]] = Left(NoneMatchShapeOr(entity, so))
        def cmb(
            v1: Either[Reason, Set[ShapeLabel]],
            current: Either[Reason, Set[ShapeLabel]]
        ): Either[Reason, Set[ShapeLabel]] =
          v1.orElse(current)
        val rs = vs.foldRight(zero)(cmb)
        rs
      case _ => Right(Set())
    }
    /*     println(s"""|checkLocal with
                     | se: ${this}
                     | entity: $entity
                     | entity local statememts: ${entity.localStatements.mkString(",")}
                     | fromLabel: $fromLabel
                     | result: ${result}
                     |""".stripMargin) */
    result
  }

  def checkLocalCoded(
      entity: Entity,
      fromLabel: ShapeLabel,
      schema: WSchema
  ): Either[ReasonCode, Set[ShapeLabel]] = {
    val result: Either[ReasonCode, Set[ShapeLabel]] = this match {
      case sr: WShapeRef =>
        schema.get(sr.label) match {
          case None => Left(Reason.shapeNotFound)
          case Some(se) =>
            se.checkLocalCoded(entity, fromLabel, schema)
        }
      case s: WShape =>
        s.expression match {
          case None     => Right(Set())
          case Some(te) =>
            /* println(s"""|CheckLocal................
                      |TripleExpr: $te
                      |""".stripMargin)   */
            te.checkLocalCoded(entity, fromLabel, s.closed, s.extras)

        }
/*      case vs: ValueSet => vs.matchLocalCoded(entity).map(_ => Set())
      case sd: StringDatatype =>
        entity match {
          //        case _: StringValue => Right(Set())
          case _ => Left(Reason.noStringDatatype)
        }
      case e: EmptyExpr => Right(Set()) */
      case nc: WNodeConstraint => nc.matchLocalCoded(entity).map(_ => Set())
      case WShapeAnd(_, ls) =>
        val vs = ls.map(_.checkLocalCoded(entity, fromLabel, schema)).sequence.map(_.toSet.flatten)
        vs
      case so @ WShapeOr(_, ls) =>
        // TODO: check the semantics...it may be enough if one of the values matches...
        val vs = ls.map(_.checkLocalCoded(entity, fromLabel, schema))
        val zero: Either[ReasonCode, Set[ShapeLabel]] = Left(Reason.noneMatchShapeOr)
        def cmb(
            v1: Either[ReasonCode, Set[ShapeLabel]],
            current: Either[ReasonCode, Set[ShapeLabel]]
        ): Either[ReasonCode, Set[ShapeLabel]] =
          v1.orElse(current)
        val rs = vs.foldRight(zero)(cmb)
        rs
      case _ => Right(Set())
    }
    /*     println(s"""|checkLocal with
                     | se: ${this}
                     | entity: $entity
                     | entity local statememts: ${entity.localStatements.mkString(",")}
                     | fromLabel: $fromLabel
                     | result: ${result}
                     |""".stripMargin) */
    result
  }

}

case class WShapeAnd(id: Option[ShapeLabel], exprs: List[WShapeExpr]) extends WShapeExpr {
  override def withLabel(label: ShapeLabel): WShapeExpr = this.copy(id = Some(label))
}

object WShapeAnd {
  def fromShapeExprs(es: List[WShapeExpr]): WShapeAnd =
    WShapeAnd(None, es)
}

case class WShapeOr(id: Option[ShapeLabel], exprs: List[WShapeExpr]) extends WShapeExpr {
  override def withLabel(label: ShapeLabel): WShapeExpr = this.copy(id = Some(label))
}

object WShapeOr {
  def fromShapeExprs(es: List[WShapeExpr]): WShapeOr =
    WShapeOr(None, es)
}

case class WShapeNot(id: Option[ShapeLabel], shapeExpr: WShapeExpr) extends WShapeExpr {
  override def withLabel(label: ShapeLabel): WShapeExpr = this.copy(id = Some(label))
}

case class WShapeRef(
  id: Option[ShapeLabel], 
  label: ShapeLabel
) extends WShapeExpr {
  override def withLabel(label: ShapeLabel): WShapeExpr = this.copy(id = Some(label))
}

case class WShape(
    id: Option[ShapeLabel],
    closed: Boolean,
    extras: List[PropertyId],
    expression: Option[TripleExpr],
    termConstraints: List[TermConstraint]
) extends WShapeExpr {

  override def withLabel(label: ShapeLabel): WShapeExpr = this.copy(id = Some(label))

  def withTermConstraints(tcs: List[TermConstraint]): WShape =
    this.copy(termConstraints = tcs)

  def withClosed(closed: Boolean): WShape = this.copy(closed = closed)
  def withExtras(es: List[PropertyId]): WShape = this.copy(extras = es)
  def withExpression(e: Option[TripleExpr]): WShape = this.copy(expression = e)
}

object WShape {
  def empty: WShape = WShape(
    id = None,
    closed = false,
    extras = List(),
    expression = None,
    termConstraints = List()
  )
}

case class WNodeConstraint(
  id: Option[ShapeLabel] = None,
  kind: Option[WNodeKind] = None, 
  datatype: Option[IRI] = None,
  xsFacets: List[XsFacet] = List(),
  values: Option[List[ValueSetValue]] = None
) extends WShapeExpr {
  override def withLabel(label: ShapeLabel): WShapeExpr = 
    this.copy(id = Some(label))
  def matchLocal(value: Value): Either[Reason, Unit] = 
    List(
      datatype.fold(().asRight[Reason])(d => matchDatatype(value,d)),
      matchFacets(value, xsFacets),
      values.fold(().asRight[Reason])(vs => matchValueSet(value, vs))
      ).sequence.map(_ => ())


  def matchLocalCoded(value: Value): Either[ReasonCode, Unit] =
    matchLocal(value).leftMap(r => r.errCode)

  private def matchKind(value: Value, kind: WNodeKind): Either[Reason,Unit] =
    kind match {
      case WNodeKind.LiteralKind => value match {
        case _: StringValue | 
             _: DateValue => ().asRight
        case _ => NotImplemented(s"LiteralKind. Failed for value: $value").asLeft
      }
      case WNodeKind.StringKind => value match {
        case _: StringValue => ().asRight
        case _ => NoStringDatatype(value).asLeft
      }
      case WNodeKind.QuantityKind   => value match {
        case _: QuantityValue => ().asRight
        case _ => NoStringDatatype(value).asLeft
      }
      case WNodeKind.TimeKind => value match {
        case _: DateValue => ().asRight
        case _            => NoDateDatatype(value).asLeft
      }
      case _ => NotImplemented(s"matchKind. Not implemented yet: $kind for value: $value").asLeft
    } 


  private def matchDatatype(value: Value, d: IRI): Either[Reason,Unit] =
    d match {
      case `xsd:string`   => value match {
        case _: StringValue => ().asRight
        case _ => NoStringDatatype(value).asLeft
      }
/*      case `xsd:integer`   => value match {
        case _: QuantityValue => ().asRight
        case _ => NoStringDatatype(value).asLeft
      } */
      case `xsd:dateTime` => value match {
        case _: DateValue => ().asRight
        case _            => NoDateDatatype(value).asLeft
      }
      case _ => UnknownDatatypeMatch(d, value).asLeft
    } 

  private def matchFacets(value: Value, facets: List[XsFacet]): Either[Reason, Unit] = 
    facets.foldRight(().asRight[Reason]) { case (facet, current) => current.combine(matchFacet(value,facet)) }

  private def matchFacet(value: Value, facet: XsFacet): Either[Reason, Unit] = 
    facet match {
      case sf: StringFacet => value match {
        case v: StringValue => FacetChecker.stringFacetChecker(v.str, sf).leftMap(StringFacetErr(_))
        case _ => StringFacetNoStringValue(sf, value).asLeft
      }
      case nf: NumericFacet => value match {
        case v: QuantityValue => 
          val numericValue = NumericDecimal(v.numericValue, v.numericValue.toString)
          FacetChecker.numericFacetChecker(numericValue, nf).leftMap(NumericFacetErr(_))
        case _ => NumericFacetNoNumericValue(nf,value).asLeft
      } 
    }

  private def matchValueSet(value: Value, vs: List[ValueSetValue]): Either[Reason, Unit] = {
    val es = vs.map(_.matchValue(value)).filter(_.isRight)
    if (es.nonEmpty) ().asRight
    else NoValueValueSet(value, vs).asLeft
  }
}


object WNodeConstraint {
  def valueSet(
    vs: List[ValueSetValue], 
    facets: List[XsFacet] = List()
    ): WNodeConstraint =
    WNodeConstraint(id = None, values = vs.some, xsFacets = facets)

  def xsFacets(fs: List[XsFacet]): WNodeConstraint = 
    WNodeConstraint(xsFacets = fs)

  def emptyExpr: WNodeConstraint = WNodeConstraint()

  def datatype(datatype: IRI, facets: List[XsFacet] = List()): WNodeConstraint =
    WNodeConstraint(id = None, datatype = datatype.some, xsFacets = facets)

  def nodeKind(kind: WNodeKind, facets: List[XsFacet] = List()): WNodeConstraint =
    WNodeConstraint(id = None, kind = kind.some, xsFacets = facets)

}

object WShapeExpr {

  def any: WShapeExpr = WShape.empty

  def label(iri: String): ShapeLabel = IRILabel(IRI(iri))

  def shapeRef(iri: String): WShapeRef = WShapeRef(None, label(iri))

  def shape(ls: List[TripleConstraint]): WShapeExpr =
    WShape(None, false, List(), Some(EachOf(exprs = ls)), List())

  def valueSet(ls: List[ValueSetValue]): WShapeExpr =
    WNodeConstraint.valueSet(ls, List())

  def qid(num: Int): ValueSetValue = {
    val name = "Q" + num
    EntityIdValueSetValue(ItemId(name, IRI(Value.siteDefault + "/" + name)))
  }
}
