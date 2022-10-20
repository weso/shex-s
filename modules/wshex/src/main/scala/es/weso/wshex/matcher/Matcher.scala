package es.weso.wshex.matcher

import es.weso.rdf.nodes._
import scala.collection.JavaConverters._
import org.wikidata.wdtk.datamodel.implementation._
import org.slf4j.LoggerFactory
import org.wikidata.wdtk.datamodel.interfaces.{
  Statement => WDTKStatement,
  StringValue => WDTKStringValue,
  Value => WDTKValue,
  Snak => WDTKSnak,
  _
}
import java.nio.file.Path
import cats.effect._
import org.wikidata.wdtk.datamodel.helpers.JsonDeserializer
import org.wikidata.wdtk.datamodel.helpers
import es.weso.utils.internal.CollectionCompat._
import es.weso.wbmodel._
import es.weso.wbmodel.Utils._
import es.weso.wshex.{NotImplemented => _, _}
import es.weso.utils.VerboseLevel
import TermConstraint._
import MatchingError._
import es.weso.rbe.interval.IntOrUnbounded
import cats.implicits._
import ReferencesSpec._
import scala.collection.JavaConverters._
import es.weso.wshex.PropertySpec.EachOfPs
import es.weso.wshex.PropertySpec.EmptySpec
import es.weso.wshex.PropertySpec.OneOfPs
import es.weso.wshex.PropertySpec.PropertyS.PropertyLocal
import es.weso.wshex.PropertySpec.PropertyS.PropertyRef

/** Matcher contains methods to match a WShEx schema with Wikibase entities
  *
  * @param wShEx schema
  * @param site URL that identifies the site. By default: http://www.wikidata.org/entity/
  * @param verbose by default false
  */
case class Matcher(
    wShEx: WSchema,
    site: String = "http://www.wikidata.org/entity/",
    verbose: VerboseLevel = VerboseLevel.Nothing
) {

  private lazy val jsonDeserializer = new helpers.JsonDeserializer(site)

  private lazy val logger = LoggerFactory.getLogger(this.getClass().getCanonicalName());

  private def info(msg: String): Unit = if (verbose.asBoolean) logger.info(msg)

  /** Checks if an entityDocument matches the start shape of a WShEx schema
    * If the schema doesn't have a start declaration, it tries to match
    * the first shape expression declared
    *
    * @param entityDocument
    * @return a matching report
    */
  def matchStart(entityDocument: EntityDocument): MatchingStatus =
    wShEx.startShapeExpr match {
      case None => NoMatching(List(NoShapeExprs(wShEx)))
      case Some(se) =>
        matchShapeExpr(
          se,
          EntityDoc(entityDocument),
          EntityDoc.emptyFrom(entityDocument)
        )
    }

  /** Match a JSON string that represents an Entity document against the start shape or the first shape
    *
    * @param jsonStr
    * @return a matching stsatus
    */
  def matchJsonStart(jsonStr: String): MatchingStatus = {
    val entityDocument = jsonDeserializer.deserializeEntityDocument(jsonStr)
    matchStart(entityDocument)
  }

  private def matchShapeExpr(
      shapeExpr: WShapeExpr,
      entity: EntityDoc,
      current: EntityDoc
  ): MatchingStatus =
    shapeExpr match {

      case s: WShape =>
        matchWShape(s, entity, current)

      case sand: WShapeAnd =>
        val ls: LazyList[MatchingStatus] =
          sand.exprs.toLazyList.map(matchShapeExpr(_, entity, current))
        MatchingStatus.combineAnds(current, ls)

      case sor: WShapeOr =>
        val ls: LazyList[MatchingStatus] =
          sor.exprs.toLazyList.map(matchShapeExpr(_, entity, current))
        MatchingStatus.combineOrs(ls)

      case snot: WShapeNot =>
        val ms = matchShapeExpr(snot.shapeExpr, entity, current)
        if (ms.matches) NoMatching(List(NotShapeFail(snot.shapeExpr, entity)))
        else
          Matching(shapeExprs = List(shapeExpr), entity = current, dependencies = ms.dependencies)

      case _ =>
        // TODO: Pending
        val notImplemented: MatchingError = NotImplemented(s"matchShape: $shapeExpr")
        NoMatching(List(notImplemented))

    }

  private def matchWShape(s: WShape, entity: EntityDoc, current: EntityDoc): MatchingStatus = {
    val matchExpr = s.expression match {
      case Some(te) => matchTripleExpr(te, entity, s, current)
      case None     => MatchingStatus.matchEmpty(current)
    }
    s.termConstraints.foldLeft(matchExpr) { case (current, tc) =>
      current match {
        case nm: NoMatching => nm // TODO: Maybe we could provide more info...
        case m: Matching    => matchTermConstraint(tc, entity, s, m.entity)
      }
    }
  }

  private def matchTermConstraint(
      tc: TermConstraint,
      entity: EntityDoc,
      s: WShape,
      current: EntityDoc
  ): MatchingStatus = tc
    .matchTerm(entity, current)
    .fold(
      err => NoMatching(List(err)),
      matched => Matching(List(s), matched)
    )

  private def matchTripleExpr(
      te: TripleExpr,
      entity: EntityDoc,
      se: WShape,
      current: EntityDoc
  ): MatchingStatus =
    te match {
      case tc: TripleConstraint =>
        matchTripleConstraint(tc, entity, se, current)

      case eo: EachOf if eo.exprs.forall(_.isInstanceOf[TripleConstraint]) =>
        val tcs: LazyList[TripleConstraint] =
          eo.exprs.map(_.asInstanceOf[TripleConstraint]).toLazyList
        MatchingStatus.combineAnds(
          current,
          tcs
            .map(tc => matchTripleConstraint(tc, entity, se, current))
        )
      case oo: OneOf if oo.exprs.forall(_.isInstanceOf[TripleConstraint]) =>
        val tcs: LazyList[TripleConstraint] =
          oo.exprs.map(_.asInstanceOf[TripleConstraint]).toLazyList
        MatchingStatus.combineOrs(
          tcs.map(tc => matchTripleConstraint(tc, entity, se, current))
        )
      case _ =>
        NoMatching(List(NotImplemented(s"matchTripleExpr: $te")))
    }

  private def matchTripleConstraint(
      tc: TripleConstraint,
      e: EntityDoc,
      se: WShape,
      current: EntityDoc
  ): MatchingStatus =
    tc match {
      case tcr: TripleConstraintRef =>
        val allowExtras = se.extras.contains(tc.property)
        val result = matchPropertyIdValueExpr(
          tc.property,
          tcr.value.some,
          e,
          se,
          current,
          tcr.min,
          tcr.max,
          allowExtras,
          tcr.qs,
          tcr.refs
        )
        result
      case tcl: TripleConstraintLocal =>
        val allowExtras = se.extras.contains(tc.property)
        val result = matchPropertyIdValueExpr(
          tc.property,
          tcl.value.some,
          e,
          se,
          current,
          tcl.min,
          tcl.max,
          allowExtras,
          tcl.qs,
          tcl.refs
        )
        result
      case tcg: TripleConstraintGeneral =>
        err(NotImplemented(s"tripleConstraintGeneral: $tcg"))
    }

  private def matchRefs(
      refSpec: ReferencesSpec,
      e: EntityDoc,
      property: PropertyId,
      se: WShapeExpr,
      current: EntityDoc
  ): MatchingStatus =
    refSpec match {
      case rs: ReferencesSpecSingle =>
        println(s"matchRefs: $rs with $property")
        val filteredStatements = e.getStatements().filter(withPropertyId(property))
        val res =
          filteredStatements
            .map { case s => (getReferences(s), s) }
            .map { case (refs, s) => matchReferencesSingle(refs, rs, s, se, current) }
        println(s"References filtered = ${filteredStatements}")
        MatchingStatus.combineAnds(current, res.toLazyList)
      case roo: ReferencesOneOf  => NoMatching(List(NotImplemented(s"ReferencesOneOf: $roo")))
      case reo: ReferencesEachOf => NoMatching(List(NotImplemented(s"ReferencesEachOf: $reo")))
    }

  def getReferences(st: WDTKStatement): LazyList[Reference] =
    st.getReferences().asScala.toLazyList

  def matchReferencesSingle(
      rs: LazyList[Reference],
      refSingle: ReferencesSpecSingle,
      st: WDTKStatement,
      se: WShapeExpr,
      current: EntityDoc
  ): MatchingStatus = {
    val (oks, errs) =
      rs.map(matchReferencePropertySpec(_, refSingle.ps, st, se, current)).partition(_.matches)
    val numOks = oks.length
    if (numOks < refSingle.min)
      NoMatching(List(ReferencesNumLessMin(numOks, refSingle.min, st, refSingle)))
    else if (refSingle.max < numOks)
      NoMatching(List(ReferencesNumGreaterMax(numOks, refSingle.max, st, refSingle)))
    else MatchingStatus.combineAnds(current, oks)
  }

  def matchReferencePropertySpec(
      ref: Reference,
      ps: PropertySpec,
      st: WDTKStatement,
      se: WShapeExpr,
      current: EntityDoc
  ): MatchingStatus =
    matchSnaksPropertySpec(ref.getAllSnaks().asScala.toList, ps, st, se, current)

  def matchSnaksPropertySpec(
      snaks: List[WDTKSnak],
      ps: PropertySpec,
      st: WDTKStatement,
      se: WShapeExpr,
      current: EntityDoc
  ): MatchingStatus =
    ps match {
      case EachOfPs(ps) => ???
      case EmptySpec =>
        if (snaks.isEmpty)
          Matching(List(se), current)
        else err(NoMatchingEmptyPropertySpec(snaks, st))
      case OneOfPs(ps) => ???
      case pl: PropertyLocal =>
        matchSnaksPropertyLocal(snaks, pl, st, se, current)
      case pr: PropertyRef => ???
    }

  private def matchSnaksPropertyLocal(
      snaks: List[WDTKSnak],
      pl: PropertyLocal,
      st: WDTKStatement,
      se: WShapeExpr,
      current: EntityDoc
  ): MatchingStatus = {
    val (oks, errs) =
      snaks.toLazyList
      .filter(hasPropertyId(pl.p, _))
      .map(matchSnakWNodeConstraint(_, pl.nc, st, se, current))
      .partition(_.matches)
     if (pl.min > oks.length)  
      err(PropertySpecLocalNumLessMin(oks.length, pl.min, pl, snaks, oks))
     else if (pl.max < oks.length) 
      err(PropertySpecLocalNumGreaterMax(oks.length, pl.max, pl, snaks, oks))
     else 
      MatchingStatus.combineAnds(current, oks)
  }

  private def matchSnakWNodeConstraint(
      snak: WDTKSnak,
      nc: WNodeConstraint,
      st: WDTKStatement,
      se: WShapeExpr,
      current: EntityDoc
  ): MatchingStatus = {
    val s = Snak.fromWDTKSnak(snak)
    nc.matchLocal(s).fold(
      err => ???,
      _ => ???
    )
  }

  private def hasPropertyId(p: PropertyId, snak: WDTKSnak): Boolean =
    snak.getPropertyId().getId() == p.id

  private def err(merr: MatchingError): MatchingStatus =
    NoMatching(List(merr))

  def withPropertyId(property: PropertyId)(st: WDTKStatement): Boolean =
    // println(s"WithPropertyId: propId = ${property.id}, statementId = ${st.getMainSnak().getPropertyId().getId()}")
    st.getMainSnak().getPropertyId().getId() == property.id

  // TODO...add matching on qualifiers
  private def matchQs(
      qs: QualifierSpec,
      e: EntityDoc,
      current: EntityDoc,
      propertyId: PropertyId
  ): MatchingStatus =
    MatchingStatus.matchEmpty(current)

  private def matchPropertyIdValueExpr(
      propertyId: PropertyId,
      valueExpr: Option[WShapeExpr],
      e: EntityDoc,
      se: WShapeExpr,
      current: EntityDoc,
      min: Int,
      max: IntOrUnbounded,
      allowExtras: Boolean,
      qs: Option[QualifierSpec],
      refs: Option[ReferencesSpec]
  ): MatchingStatus = {
    val predicate = propertyId.iri
    val pidValue: PropertyIdValue = predicate2propertyIdValue(predicate)
    val values = e.getValues(pidValue)
    println(s"## MatchPropertyIdValueExpr: $predicate value: $pidValue")
    val resValueExpr = valueExpr match {
      case None =>
        val valuesCounter = values.length
        if (min > valuesCounter) {
          NoMatching(List(ValuesPropertyFailMin(predicate, e, valuesCounter, min)))
        } else if (max < valuesCounter) {
          NoMatching(List(ValuesPropertyFailMax(predicate, e, valuesCounter, max)))
        } else
          Matching(
            shapeExprs = List(se),
            entity = current.addPropertyValues(pidValue, values)
          )

      case Some(nse) =>
        nse match {
          case wnc: WNodeConstraint =>
            // println(s"Matching WNodeConstraint: $wnc with predicate $predicate")
            matchPredicateWNodeConstraintValues(
              wnc,
              values,
              current,
              se,
              pidValue,
              min,
              max,
              allowExtras,
              qs,
              refs
            )
          case _ =>
            NoMatching(
              List(
                NotImplemented(s"matchPropertyIdValueExpr: ${predicate}, valueExpr: ${valueExpr}")
              )
            )
        }
    }
    val resQs =
      qs.fold(resValueExpr)(quals => 
          resValueExpr.and(matchQs(quals, e, current, propertyId)))
    val resRefs = 
       refs.fold(resQs)(refs => 
          resQs.and(matchRefs(refs, e, propertyId, se, current)))
    println(s"After checking references: $resRefs")
    resRefs
  }

  private def matchPredicateWNodeConstraintValues(
      wnc: WNodeConstraint,
      values: LazyList[WDTKValue],
      current: EntityDoc,
      se: WShapeExpr,
      pidValue: PropertyIdValue,
      min: Int,
      max: IntOrUnbounded,
      allowExtras: Boolean,
      qs: Option[QualifierSpec],
      refs: Option[ReferencesSpec]
  ): MatchingStatus = {
    val (matched, noMatched) =
      values
        .map(matchPredicateWNodeConstraintValue(wnc, _, current, se, pidValue, qs, refs))
        .partition(_.matches)
    val matchedCount = matched.length
    if (matchedCount < min) {
      NoMatching(
        List(
          ValuesPropertyFailNodeConstraintMin(pidValue, matchedCount, min, wnc, noMatched, matched)
        )
      )
    } else if (max < matchedCount) {
      NoMatching(
        List(ValuesPropertyFailNodeConstraintMax(pidValue, matchedCount, max, wnc, matched))
      )
    } else {
      if (noMatched.nonEmpty) {
        if (!allowExtras)
          NoMatching(List(ValuesPropertyFailNodeConstraint(pidValue, wnc, noMatched)))
        else
          // TODO. noMatched contains values but EXTRA is allowed...should we add those extra values?
          MatchingStatus.combineAnds(current, matched)
      } else {
        MatchingStatus.combineAnds(current, matched)
      }
    }
  }

  private def matchPredicateWNodeConstraintValue(
      wnc: WNodeConstraint,
      value: WDTKValue,
      current: EntityDoc,
      se: WShapeExpr,
      pidValue: PropertyIdValue,
      qs: Option[QualifierSpec],
      refs: Option[ReferencesSpec]
  ): MatchingStatus = {
    // TODO: Add Qs and referencesSpec
    val wbValue = Value.fromWDTKValue(value)
    wnc
      .matchLocal(wbValue)
      .fold(
        reason => NoMatching(List(WNodeConstraintError(reason, value, wbValue))),
        _ => Matching(List(se), 
        current.addPropertyValues(pidValue, LazyList(value)))
      )
  }

  private def predicate2propertyIdValue(predicate: IRI): PropertyIdValue = {
    val (localName, base) = splitIri(predicate)
    val propertyId = new PropertyIdValueImpl(localName, base)
    propertyId
  }

}

object Matcher {

  val defaultEntityIRI = es.weso.wbmodel.Value.defaultIRI

  /** Read a WShEx from a path
    *
    * @param schemaPath: Path where the WShEx schema can be found
    * @param verbose: Print more messages during validation
    * @param format: WShEx format
    * @return an IO action that returns a matcher
    */
  def fromPath(
      schemaPath: Path,
      format: WShExFormat = WShExFormat.ESCompactFormat,
      base: Option[IRI] = None,
      entityIRI: IRI = defaultEntityIRI,
      verbose: VerboseLevel = VerboseLevel.Nothing
  ): IO[Matcher] =
    WSchema
      .fromPath(schemaPath, format, base, entityIRI, verbose)
      .map(s => Matcher(wShEx = s, verbose = verbose))

  /** Read a WShEx from a path
    * This is the synchronous and unsafe version
    * An IO-based version is also available
    *
    * @param schemaPath: Path where the WShEx schema can be found
    * @param verbose: Print more messages during validation
    * @param format: WShEx format
    * @return a matcher
    */
  def unsafeFromPath(
      schemaPath: Path,
      format: WShExFormat = WShExFormat.CompactWShExFormat,
      base: Option[IRI] = None,
      entityIRI: IRI = defaultEntityIRI,
      verbose: VerboseLevel = VerboseLevel.Nothing
  ): Matcher = {
    import cats.effect.unsafe.implicits.global
    fromPath(schemaPath, format, base, entityIRI, verbose).unsafeRunSync()
  }

  def unsafeFromString(
      str: String,
      verbose: VerboseLevel = VerboseLevel.Nothing,
      base: Option[IRI] = None,
      entityIRI: IRI = defaultIRI,
      format: WShExFormat = WShExFormat.CompactWShExFormat
  ): Either[ParseError, Matcher] =
    WSchema
      .unsafeFromString(str, format, base, entityIRI, verbose)
      .map(s => Matcher(wShEx = s, verbose = verbose))

  val defaultIRI = es.weso.wbmodel.Value.defaultIRI

}
