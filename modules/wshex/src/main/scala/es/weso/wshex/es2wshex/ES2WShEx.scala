package es.weso.wshex.es2wshex

import cats._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso._
import es.weso.rbe.interval.{IntLimit, Unbounded}
import es.weso.rdf.nodes._
import es.weso.rdf.{Prefix, PrefixMap}
import es.weso.wbmodel.{Property => _, _}
import es.weso.rbe.interval.IntOrUnbounded
import scala.collection.compat._ // Required for partitionMap
import es.weso.rdf.nodes._
import es.weso.wshex._
import es.weso.wshex.TermConstraint._
import es.weso.wshex.ListSpec.Single
import es.weso.wshex.ReferencesSpec._
import es.weso.wshex.PropertySpec._
import es.weso.wshex.PropertySpec.PropertyConstraint._

case class ES2WShEx(convertOptions: ES2WShExConvertOptions) extends LazyLogging {

  type Convert[A] = Either[ES2WShExConvertError, A]
  private def ok[A](x: A): Convert[A] = x.asRight
  private def err[A](x: ES2WShExConvertError): Convert[A] = x.asLeft

  // Properties for labels
  private lazy val rdfs = IRI("http://www.w3.org/2000/01/rdf-schema#")
  private lazy val schema = IRI("http://schema.org/")
  private lazy val skos = IRI("http://www.w3.org/2004/02/skos/core#")
  private lazy val rdfsLabel = rdfs + "label"
  private lazy val schemaName = schema + "name"
  private lazy val skosPrefLabel = skos + "prefLabel"

  // Properties for descriptions
  private lazy val schemaDescription = schema + "description"
  // Properties for aliases
  private lazy val skosAltLabel = skos + "altLabel"

  private lazy val termPredicates: List[IRI] = List(
    rdfsLabel,
    schemaName,
    skosPrefLabel,
    schemaDescription,
    skosAltLabel
  )

  /** Convert an entity schema in ShEx to WShEx
    */
  def convert(
      shexSchema: shex.AbstractSchema
  ): Convert[WSchema] =
    for {
      shapes <-
        shexSchema.shapesMap.toList.map { case (l, se) =>
          convertLabelShapeExpr(l, se, shexSchema)
        }.sequence

      start <- shexSchema.start match {
        case None     => none.asRight
        case Some(se) => convertShapeExpr(se, shexSchema).flatMap(se => Right(Some(se)))
      }
      prefixes <- convertPrefixes(shexSchema.prefixes)
    } yield WSchema(
      shapesMap = shapes.toMap,
      start = start,
      prefixes = prefixes,
      base = shexSchema.base
    )

  private def convertPrefixes(maybePm: Option[PrefixMap]): Convert[Option[PrefixMap]] =
    maybePm.fold(ok(none[PrefixMap]))(pm => ok(removeKnownPrefixes(pm).some))

  private def removeKnownPrefixes(pm: PrefixMap): PrefixMap = {
    def knownIris = List(
      convertOptions.entityIri,
      convertOptions.directPropertyIri,
      convertOptions.propIri,
      convertOptions.propStatementIri,
      convertOptions.propQualifierIri,
      convertOptions.propReferenceIri,
      skos,
      rdfs,
      schema
    )
    def notKnown(alias: Prefix, iri: IRI): Boolean =
      !knownIris.contains(iri)

    PrefixMap(pm.pm.filter { case (alias, iri) => notKnown(alias, iri) })
      .addPrefix("", convertOptions.entityIri)
  }

  private def convertLabelShapeExpr(
      label: shex.ShapeLabel,
      se: shex.ShapeExpr,
      shexSchema: shex.AbstractSchema
  ): Convert[(ShapeLabel, WShapeExpr)] = for {
    cse <- convertShapeExpr(se, shexSchema)
    lbl = convertShapeLabel(label)
  } yield (lbl, cse)

  private def convertShapeExpr(
      se: shex.ShapeExpr,
      schema: shex.AbstractSchema
  ): Convert[WShapeExpr] =
    se match {
      case nc: shex.NodeConstraint => convertNodeConstraint(nc)
      case s: shex.Shape           => convertShape(s, schema)
      case sand: shex.ShapeAnd =>
        for {
          ss <- sand.shapeExprs.map(convertShapeExpr(_, schema)).sequence
        } yield WShapeAnd(id = convertId(sand.id), exprs = ss)
      case sor: shex.ShapeOr =>
        for {
          ss <- sor.shapeExprs.map(convertShapeExpr(_, schema)).sequence
        } yield WShapeOr(id = convertId(sor.id), exprs = ss)
      case snot: shex.ShapeNot =>
        convertShapeExpr(snot.shapeExpr, schema)
          .map(se => WShapeNot(id = convertId(snot.id), shapeExpr = se))
      case sref: shex.ShapeRef =>
        WShapeRef(convertId(sref.id), convertShapeLabel(sref.reference)).asRight
      case sd: shex.ShapeDecl =>
        convertShapeExpr(sd.shapeExpr, schema).map(_.withLabel(convertShapeLabel(sd.lbl)))
      case _ =>
        err(UnsupportedShapeExpr(se))
    }

  private def convertId(id: Option[shex.ShapeLabel]): Option[ShapeLabel] =
    id.map(convertShapeLabel)

  private def convertNodeConstraint(
      nc: shex.NodeConstraint
  ): Convert[WNodeConstraint] =
    nc match {
      case shex.NodeConstraint(id, None, None, List(), None, None, None) =>
        ok(WNodeConstraint.emptyExpr)
      case shex.NodeConstraint(id, None, None, List(), Some(values), None, None) =>
        convertValueSet(convertId(id), values)
      case _ =>
        println(s"Node constraint: $nc")
        err(UnsupportedNodeConstraint(nc))
    }

  private def convertValueSet(
      id: Option[ShapeLabel],
      values: List[shex.ValueSetValue]
  ): Convert[WNodeConstraint] =
    convertValueSetValues(values)
      .map(vs => WNodeConstraint(id = id, values = vs.some))

  private def convertValueSetValues(
      values: List[shex.ValueSetValue]
  ): Either[ES2WShExConvertError, List[ValueSetValue]] =
    values
      .map(convertValueSetValue)
      .sequence

  private def convertValueSetValue(
      value: shex.ValueSetValue
  ): Convert[ValueSetValue] =
    value match {
      case shex.IRIValue(i) =>
        val (name1, base1) = Utils.splitIri(i)
        logger.trace(s"""|convertValueSetValue:
              |name1: $name1
              |base1: $base1
              |""".stripMargin)
        if (IRI(base1) == convertOptions.entityIri) {
          EntityId
            .fromIri(i)
            .leftMap(ErrorConvertingIRI(_))
            .map(EntityIdValueSetValue(_))
        } else {
          IRIValueSetValue(i).asRight
        }
      case shex.IRIStem(s) => IRIStem(s).asRight
      case _               => UnsupportedValueSetValue(value).asLeft
    }

  private def convertShape(
      s: shex.Shape,
      schema: shex.AbstractSchema
  ): Convert[WShape] =
    for {
      te <- optConvert(s.expression, convertTripleExpr(schema))
      ls <- s.expression match {
        case None     => List().asRight
        case Some(ts) => parseTermsExpr(ts)
      }
      extras <- {
        val es = s.extra.getOrElse(List())
        es.map(convertPropertyIRIExtra(_)).sequence
      }
    } yield WShape(
      id = convertId(s.id),
      closed = s.closed.getOrElse(false),
      extras = extras,
      expression = te,
      ls
    )

  private def convertPropertyIRIExtra(iri: IRI): Convert[PropertyId] = {
    val iriParsed = IRIConvert.parseIRI(iri, convertOptions)
    iriParsed match {
      case Some(DirectProperty(n)) => PropertyId.fromNumber(n, convertOptions.entityIri).asRight
      case Some(Property(n))       => PropertyId.fromNumber(n, convertOptions.entityIri).asRight
      case _                       => UnsupportedExtraProperty(iri).asLeft
    }
  }

  private def parseTermsExpr(
      te: shex.TripleExpr
  ): Either[ES2WShExConvertError, List[TermConstraint]] =
    te match {
      case eo: shex.EachOf =>
        eo.expressions.map(parseTermsExpr(_)).sequence.map(_.flatten)
      case tc: shex.TripleConstraint => parseTermTripleConstraint(tc)
      case oo: shex.OneOf => List().asRight // We ignore term declarations embedded in OneOf
      case i: shex.Inclusion =>
        List().asRight // We ignore term declarations embedded in Inclusions
      case _ => List().asRight // We ignore term declarations in other triple expressions...
    }

  private def parseTermTripleConstraint(
      tc: shex.TripleConstraint
  ): Convert[List[TermConstraint]] = tc.predicate match {

    case `rdfsLabel` =>
      tc.valueExpr match {
        case None => List(LabelAny(None)).asRight
        case Some(v) =>
          v match {
            case nc: shex.NodeConstraint =>
              nc.values match {
                case None => List(LabelAny(None)).asRight
                case Some(vs) =>
                  val parseLangValues = vs.map { case v =>
                    v match {
                      case shex.Language(l) => Lang(l.lang).asRight
                      case _                => UnsupportedValueSetValue(v).asLeft
                    }
                  }.sequence
                  val noConstraint = none[StringConstraint]
                  parseLangValues.map(vs =>
                    List(LabelConstraint(vs.map(v => (v, noConstraint)).toMap))
                  )
              }
          }
      }

    case `schemaDescription` =>
      tc.valueExpr match {
        case None => List(DescriptionAny(None)).asRight
        case Some(v) =>
          v match {
            case nc: shex.NodeConstraint =>
              nc.values match {
                case None => List(DescriptionAny(None)).asRight
                case Some(vs) =>
                  val parsedLangValues = vs.map { case v =>
                    v match {
                      case shex.Language(l) => Lang(l.lang).asRight
                      case _                => UnsupportedValueSetValue(v).asLeft
                    }
                  }.sequence
                  val noConstraint = none[StringConstraint]
                  parsedLangValues.map(vs =>
                    List(DescriptionConstraint(vs.map(v => (v, noConstraint)).toMap))
                  )
              }
          }
      }

    case `skosAltLabel` =>
      tc.valueExpr match {
        case None => List(AliasAny(None)).asRight
        case Some(v) =>
          v match {
            case nc: shex.NodeConstraint =>
              nc.values match {
                case None => List(AliasAny(None)).asRight
                case Some(vs) =>
                  val parsedValues = vs.map { case v =>
                    v match {
                      case shex.Language(l) => Lang(l.lang).asRight
                      case _                => UnsupportedValueSetValue(v).asLeft
                    }
                  }.sequence
                  val noConstraint = none[StringConstraint]
                  parsedValues.map(vs =>
                    List(AliasConstraint(vs.map(v => (v, noConstraint)).toMap))
                  )
              }
          }
      }
    case _ => List().asRight
  }

  private def optConvert[A, B](
      v: Option[A],
      cnv: A => Convert[Option[B]]
  ): Either[ES2WShExConvertError, Option[B]] =
    v.fold(none[B].asRight[ES2WShExConvertError])(cnv(_))

  private def convertTripleExpr(
      schema: shex.AbstractSchema
  )(te: shex.TripleExpr): Convert[Option[TripleExpr]] =
    te match {
      case eo: shex.EachOf =>
        eo.expressions
          .map(convertTripleExpr(schema))
          .sequence
          .map(_.flatten)
          .map(_ match {
            case Nil       => none[TripleExpr]
            case te :: Nil => te.some
            case tes       => EachOf(exprs = tes).some
          })

      case oo: shex.OneOf =>
        oo.expressions
          .map(convertTripleExpr(schema))
          .sequence
          .map(_.flatten)
          .map(_ match {
            case Nil => none[TripleExpr]
            case tes => OneOf(exprs = tes).some
          })
      case tc: shex.TripleConstraint =>
        convertTripleConstraint(tc, schema)
      case _ =>
        logger.warn(s"Unsupported triple expression: $te")
        Left(UnsupportedTripleExpr(te))
    }

  private def makeTripleConstraint(
      pred: PropertyId,
      min: Int,
      max: IntOrUnbounded,
      se: Option[shex.ShapeExpr],
      schema: shex.AbstractSchema
  ): Convert[TripleConstraint] =
    se match {
      case None =>
        TripleConstraintLocal(pred, WNodeConstraint.emptyExpr, min, max).asRight
      case Some(se) =>
        convertShapeExpr(se, schema).flatMap(s =>
          s match {
            case s @ WShapeRef(_, lbl) =>
              TripleConstraintRef(pred, s, min, max, None).asRight
            case wnc: WNodeConstraint =>
              TripleConstraintLocal(pred, wnc, min, max).asRight
            case _ =>
              UnsupportedShapeExpr(se, s"Making tripleConstraint for pred: $pred").asLeft
          }
        )
    }

  private def convertTripleConstraint(
      tc: shex.TripleConstraint,
      schema: shex.AbstractSchema
  ): Convert[Option[TripleConstraint]] = {
    val iriParsed = IRIConvert.parseIRI(tc.predicate, convertOptions)
    iriParsed match {
      case Some(DirectProperty(n)) =>
        val pred = PropertyId.fromNumber(n, convertOptions.entityIri)
        val (min, max) = convertMinMax(tc)
        makeTripleConstraint(pred, min, max, tc.valueExpr, schema).map(_.some)

      case Some(Property(p)) =>
        tc.valueExpr match {
          case Some(ve) => convertTripleConstraintProperty(p, ve, schema)
          case None     => err(NoValueForPropertyConstraint(p, tc))
        }

      case Some(PropertyQualifier(n)) =>
        // val pred = PropertyId.fromNumber(n, convertOptions.entityIri)
        // val (min, max) = convertMinMax(tc)
        // makeTripleConstraint(pred, min, max, tc.valueExpr, schema).map(_.some)
        ok(none)

      case Some(PropertyStatement(n)) =>
        // val pred = PropertyId.fromNumber(n, convertOptions.entityIri)
        // val (min, max) = convertMinMax(tc)
        // makeTripleConstraint(pred, min, max, tc.valueExpr, schema).map(_.some)
        ok(none)

      case Some(WasDerivedFrom) =>
        ok(none)

      case Some(PropertyReference(n)) =>
        // val pred = PropertyId.fromNumber(n, convertOptions.entityIri)
        // val (min, max) = convertMinMax(tc)
        // makeTripleConstraint(pred, min, max, tc.valueExpr, schema).map(_.some)
        ok(none)

      case _ =>
        if (termPredicates.contains(tc.predicate))
          ok(none)
        else
          err(UnsupportedPredicate(tc.predicate, s"Parsing direct tripleConstraint $tc"))
    }
  }

  private def convertMax(max: shex.Max): IntOrUnbounded = max match {
    case shex.Star      => Unbounded
    case shex.IntMax(m) => IntLimit(m)
  }

  private def convertMinMax(tc: shex.TripleConstraint): (Int, IntOrUnbounded) = {
    val min = tc.min
    val max = convertMax(tc.max)
    (min, max)
  }

  private def convertTripleConstraintProperty(
      n: Int,
      shapeExpr: shex.ShapeExpr,
      schema: shex.AbstractSchema
  ): Convert[Option[TripleConstraint]] =
    shapeExpr match {
      case s: shex.Shape =>
        convertTripleConstraintPropertyShape(n, s, schema)
      case sd: shex.ShapeDecl =>
        convertTripleConstraintProperty(n, sd.shapeExpr, schema)
      case ref: shex.ShapeRef =>
        schema.getShape(ref.reference) match {
          case Left(msg) => NotFoundShape(ref.reference, msg).asLeft
          case Right(se) =>
            se match {
              case s: shex.Shape =>
                convertTripleConstraintPropertyShape(n, s, schema)
              case s: shex.ShapeDecl =>
                convertTripleConstraintProperty(n, s.shapeExpr, schema)
              case _ =>
                UnsupportedShapeExpr(
                  se,
                  s"Parsing property $n with ref ${ref.reference} and se= $se"
                ).asLeft
            }
        }
      case _ => UnsupportedShapeExpr(shapeExpr, s"Parsing property $n").asLeft
    }

  private def convertTripleConstraintPropertyShape(
      n: Int,
      s: shex.Shape,
      schema: shex.AbstractSchema
  ): Convert[Option[TripleConstraint]] =
    s.expression match {
      case None => NoExprForTripleConstraintProperty(n, s).asLeft
      case Some(te) =>
        te match {
          case tc: shex.TripleConstraint =>
            val iriParsed = IRIConvert.parseIRI(tc.predicate, convertOptions)
            val pred = PropertyId.fromNumber(n, convertOptions.entityIri)
            val (min, max) = convertMinMax(tc)
            iriParsed match {
              case Some(PropertyStatement(ns)) =>
                if (n == ns) {
                  makeTripleConstraint(pred, min, max, tc.valueExpr, schema)
                    .map(_.some)
                } else
                  DifferentPropertyPropertyStatement(n, ns).asLeft
              case Some(PropertyReference(n)) =>
                makeTripleConstraint(pred, min, max, shex.NodeConstraint.empty.some, schema)
                  .map(_.some)
              // none.asRight // NoExprForTripleConstraintProperty(n, s).asLeft
              case Some(WasDerivedFrom) =>
                makeTripleConstraint(pred, min, max, shex.NodeConstraint.empty.some, schema)
                  .map(_.some)
              // none.asRight // NoExprForTripleConstraintProperty(n, s).asLeft
              case _ =>
                UnsupportedPredicate(
                  tc.predicate,
                  s"Parsing shape for property $n\nShape: ${s}"
                ).asLeft
            }
          case s: shex.EachOf =>
            parseEachOfForProperty(n, s, schema)
              .map(_.some)
          case _ =>
            UnsupportedTripleExpr(te, s"Parsing property $n").asLeft
        }
    }

  private def convertShapeLabel(label: shex.ShapeLabel): ShapeLabel =
    label match {
      case shex.IRILabel(iri)     => IRILabel(iri)
      case shex.BNodeLabel(bnode) => BNodeLabel(bnode)
      case shex.Start             => Start
    }

  private def parseEachOfForProperty(
      n: Int,
      s: shex.EachOf,
      schema: shex.AbstractSchema
  ): Convert[TripleConstraint] =
    getPropertyStatement(n, s.expressions, schema).flatMap(tc =>
      getQualifiers(s.expressions, n, schema).flatMap(qs =>
        getReferencesFromWasDerivedFrom(s.expressions, n, schema).flatMap(refs =>
          tc.withQs(qs).withRefs(refs).asRight
        )
      )
    )

  private def getPropertyStatement(
      n: Int,
      es: List[shex.TripleExpr],
      schema: shex.AbstractSchema
  ): Convert[TripleConstraint] =
    es.collectFirstSome(checkPropertyStatement(n, schema)) match {
      case None     => err(NoValueForPropertyStatementExprs(n, es))
      case Some(tc) => ok(tc)
    }

  private def checkPropertyStatement(n: Int, schema: shex.AbstractSchema)(
      te: shex.TripleExpr
  ): Option[TripleConstraint] =
    te match {
      case tc: shex.TripleConstraint =>
        val iriParsed = IRIConvert.parseIRI(tc.predicate, convertOptions)
        iriParsed match {
          case Some(PropertyStatement(ns)) =>
            if (n == ns) {
              val pred = PropertyId.fromNumber(n, convertOptions.entityIri)
              val (min, max) = convertMinMax(tc)
              makeTripleConstraint(pred, min, max, tc.valueExpr, schema).toOption
            } else None
          case _ => None
        }
      case _ => None
    }

  private def getQualifiers(
      es: List[shex.TripleExpr],
      n: Int,
      schema: shex.AbstractSchema
  ): Convert[Option[QualifierSpec]] = {
    val (errs, oks) = es.map(getQualifier(n, schema)).partitionMap(x => x)
    if (errs.isEmpty) {
      val vs = oks.flatten
      if (vs.isEmpty) ok(none)
      else
        ok(QualifierSpec(EachOfPs(vs, 1, IntLimit(1)), false).some)
    } else {
      err(ConvertErrors(errs))
    }
  }

  private def getReferencesFromWasDerivedFrom(
      es: List[shex.TripleExpr],
      n: Int,
      schema: shex.AbstractSchema
  ): Convert[Option[ReferencesSpec]] = {
    println(s"getReferences of property $n, es = $es")
    val refs: Convert[List[ReferencesSpec]] = es.collect {
      case tc: shex.TripleConstraint if tc.predicate == `prov:wasDerivedFrom` =>
        getReferences(tc.valueExpr, tc.optMin, tc.optMax, n, schema)
    }.sequence

    refs.flatMap(_ match {
      case Nil        => ok(none[ReferencesSpec])
      case ref :: Nil => ok(ref.some)
      case refs       => ok(ReferencesEachOf(refs).some)
    })
  }

  private def getReferences(
      optSe: Option[shex.ShapeExpr],
      optMin: Option[Int],
      optMax: Option[shex.Max],
      n: Int,
      schema: shex.AbstractSchema
  ): Convert[ReferencesSpecSingle] = {
    val min = optMin.getOrElse(defaultMin)
    val max = optMax.map(convertMax).getOrElse(defaultMax)
    optSe match {
      case None     => ok(ReferencesSpecSingle(PropertySpec.EmptySpec, min, max, false))
      case Some(se) => getReferencesShapeExpr(se, min, max, n, schema)
    }
  }

  private def getReferencesShapeExpr(
      se: shex.ShapeExpr,
      min: Int,
      max: IntOrUnbounded,
      n: Int,
      schema: shex.AbstractSchema
  ): Convert[ReferencesSpecSingle] = se match {
    case ref: shex.ShapeRef =>
      schema.getShape(ref.reference) match {
        case Left(msg) => NotFoundShape(ref.reference, msg).asLeft
        case Right(se) =>
          se match {
            case s: shex.Shape =>
              convertPropertySpecFromShape(n, s, schema).flatMap(ps =>
                ok(ReferencesSpecSingle(ps, min, max, false))
              )
            case s: shex.ShapeDecl =>
              getReferencesShapeExpr(se, min, max, n, schema)
            case _ =>
              err(
                UnsupportedShapeExpr(
                  se,
                  s"Parsing wasDerivedFrom $n with ref ${ref.reference} and se= $se"
                )
              )
          }
      }
    case sd: shex.ShapeDecl => getReferencesShapeExpr(sd.shapeExpr, min, max, n, schema)
    case s: shex.Shape =>
      convertPropertySpecFromShape(n, s, schema).flatMap(ps =>
        ok(ReferencesSpecSingle(ps, min, max, false))
      )
    case _ => err(UnsupportedShapeExprWasDerivedFrom(n, se))
  }

  private def convertPropertySpecFromShape(
      n: Int,
      shape: shex.Shape,
      schema: shex.AbstractSchema
  ): Convert[PropertySpec] =
    shape.expression match {
      case None     => ok(PropertySpec.EmptySpec)
      case Some(te) => getPropertySpec(n, schema)(te).map(_.getOrElse(PropertySpec.EmptySpec))
    }

  private def checkAllSome[A](ls: List[Option[A]])(e: ES2WShExConvertError): Convert[List[A]] =
    ls.sequence match {
      case None     => err(e)
      case Some(ls) => ok(ls)
    }

  private def getPropertySpec(n: Int, schema: shex.AbstractSchema)(
      te: shex.TripleExpr
  ): Convert[Option[PropertySpec]] = te match {
    case tc: shex.TripleConstraint =>
      convertTripleConstraintPropertySpec(n, schema, tc)
    case eo: shex.EachOf =>
      for {
        maybeExprs <- eo.expressions.map(getPropertySpec(n, schema)(_)).sequence
        exprs <- checkAllSome(maybeExprs)(ErrorParsingPropretySpecNone(n, te, maybeExprs))
      } yield EachOfPs(exprs, eo.min, convertMax(eo.max)).some

    case oo: shex.OneOf =>
      for {
        maybeExprs <- oo.expressions.map(getPropertySpec(n, schema)(_)).sequence
        exprs <- checkAllSome(maybeExprs)(ErrorParsingPropretySpecNone(n, te, maybeExprs))
      } yield OneOfPs(exprs, oo.min, convertMax(oo.max)).some

    case _ => UnsupportedTripleExpr(te, s"Parsing propertySpec for property $n").asLeft
  }

  private def convertTripleConstraintPropertySpec(
      n: Int,
      schema: shex.AbstractSchema,
      tc: shex.TripleConstraint
  ): Convert[Option[PropertyConstraint]] = {
    val iriParsed = IRIConvert.parseIRI(tc.predicate, convertOptions)
    iriParsed match {
      case Some(PropertyReference(nr)) =>
        val pr = PropertyId.fromNumber(nr, convertOptions.propReferenceIri)
        val (min, max) = convertMinMax(tc)
        tc.valueExpr match {
          case None => PropertyLocal(pr, WNodeConstraint.emptyExpr, min, max).some.asRight
          case Some(se) =>
            convertShapeExpr(se, schema).flatMap(s =>
              s match {
                case s @ WShapeRef(_, lbl) => PropertyRef(pr, s, min, max).some.asRight
                case wnc: WNodeConstraint  => PropertyLocal(pr, wnc, min, max).some.asRight
                case _ =>
                  UnsupportedShapeExpr(se, s"Parsing property references for property $n").asLeft
              }
            )
        }
      case _ => UnsupportedPredicate(tc.predicate, s"Parsing references for property $n").asLeft
    }
  }

  private def getQualifier(n: Int, schema: shex.AbstractSchema)(
      te: shex.TripleExpr
  ): Either[ES2WShExConvertError, Option[PropertyConstraint]] =
    te match {
      case tc: shex.TripleConstraint =>
        val iriParsed = IRIConvert.parseIRI(tc.predicate, convertOptions)
        iriParsed match {
          case Some(PropertyStatement(ns)) =>
            if (n == ns) none.asRight
            else DifferentPropertyPropertyStatement(n, ns, s"Parsing qualifiers").asLeft
          case Some(PropertyQualifier(nq)) =>
            val pq = PropertyId.fromNumber(nq, convertOptions.propQualifierIri)
            val (min, max) = convertMinMax(tc)
            tc.valueExpr match {
              case None => PropertyLocal(pq, WNodeConstraint.emptyExpr, min, max).some.asRight
              case Some(se) =>
                convertShapeExpr(se, schema).flatMap(s =>
                  s match {
                    case s @ WShapeRef(_, lbl) => PropertyRef(pq, s, min, max).some.asRight
                    case wnc: WNodeConstraint  => PropertyLocal(pq, wnc, min, max).some.asRight
                    case _ =>
                      UnsupportedShapeExpr(se, s"Parsing qualifiers for property $n").asLeft
                  }
                )
            }
          case Some(WasDerivedFrom) => none.asRight
          case _ => UnsupportedPredicate(tc.predicate, s"Parsing qualifiers for property $n").asLeft
        }
      case _ => UnsupportedTripleExpr(te, s"Parsing qualifiers of property $n").asLeft
    }
}

object ES2WShEx {
  def apply(
      convertOptions: ES2WShExConvertOptions = ES2WShExConvertOptions.default
  ): ES2WShEx =
    // Note: 'new' is needed to avoid infinite loop
    new ES2WShEx(convertOptions)

}
