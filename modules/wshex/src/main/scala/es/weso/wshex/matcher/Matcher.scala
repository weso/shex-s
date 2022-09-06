package es.weso.wshex.matcher

import es.weso.rdf.nodes._
import org.wikidata.wdtk.datamodel.interfaces._

import scala.collection.JavaConverters._
import org.wikidata.wdtk.datamodel.implementation._
import org.slf4j.LoggerFactory
import org.wikidata.wdtk.datamodel.interfaces.{
  Statement => WDTKStatement,
  StringValue => WDTKStringValue,
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
        MatchingStatus.combineOrs(current, ls)

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
    // val ms =
    //  s.termConstraints.map(tc => matchTermConstraint(tc, entity, s, current))
    // ms.foldLeft(matchExpr) { case (c, current) => c.and(current) }
    s.termConstraints.foldLeft(matchExpr) { case (current, tc) => {
      current match {
        case nm: NoMatching => nm // TODO: Maybe we could provide more info...
        case m: Matching => matchTermConstraint(tc, entity, s, m.entity)
      }
    }}
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
      se: WShapeExpr,
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
          current,
          tcs.map(tc => matchTripleConstraint(tc, entity, se, current))
        )
      case _ =>
        NoMatching(List(NotImplemented(s"matchTripleExpr: $te")))
    }

  private def matchTripleConstraint(
      tc: TripleConstraint,
      e: EntityDoc,
      se: WShapeExpr,
      current: EntityDoc
  ): MatchingStatus =
    tc match {
      case tcr: TripleConstraintRef =>
        val matchPid = matchPropertyIdValueExpr(tc.property, Some(tcr.value), e, se, current)
        tcr.qs match {
          case None     => matchPid
          case Some(qs) => matchPid.and(matchQs(qs, e, current))
        }
      case tcl: TripleConstraintLocal =>
        val matchPid = matchPropertyIdValueExpr(tc.property, Some(tcl.value), e, se, current)
        tcl.qs match {
          case None     => matchPid
          case Some(qs) => matchPid.and(matchQs(qs, e, current))
        }
      case tcg: TripleConstraintGeneral =>
        val notImplemented: MatchingError = NotImplemented(s"tripleConstraintGeneral: $tcg")
        NoMatching(List(notImplemented))
    }

  // TODO...add matching on qualifiers
  private def matchQs(
      qs: QualifierSpec,
      e: EntityDoc,
      current: EntityDoc
  ): MatchingStatus =
    MatchingStatus.matchEmpty(current)

  private def matchPropertyIdValueExpr(
      propertyId: PropertyId,
      valueExpr: Option[WShapeExpr],
      e: EntityDoc,
      se: WShapeExpr,
      current: EntityDoc
  ): MatchingStatus = {
    val predicate = propertyId.iri
    val pidValue: PropertyIdValue = predicate2propertyIdValue(predicate)
    val values = e.getValues(pidValue)
    valueExpr match {
      case None =>
        if (values.isEmpty) NoMatching(List(NoValuesProperty(predicate, e)))
        else
          Matching(
            shapeExprs = List(se),
            entity = current.addPropertyValues(pidValue, values)
          )

      case Some(ValueSet(_, vs)) =>
        MatchingStatus
          .combineOrs(
            current,
            vs.toLazyList
              .map(matchPredicateValueSetValue(predicate, _, e, se, current))
          )
      case Some(EmptyExpr(_)) =>
        if (values.isEmpty) NoMatching(List(NoValuesProperty(predicate, e)))
        else
          Matching(
            shapeExprs = List(se),
            entity = current.addPropertyValues(pidValue, values)
          )
      case _ =>
        NoMatching(
          List(NotImplemented(s"matchPropertyIdValueExpr: ${predicate}, valueExpr: ${valueExpr}"))
        )
    }
  }

  private def matchPredicateValueSetValue(
      predicate: IRI,
      value: ValueSetValue,
      e: EntityDoc,
      se: WShapeExpr,
      current: EntityDoc
  ) =
    value match {
      case IRIValueSetValue(iri) => matchPredicateIri(predicate, iri, e.entityDocument, se, current)
      case EntityIdValueSetValue(id) =>
        matchPredicateIri(predicate, id.iri, e.entityDocument, se, current)
      case _ =>
        NoMatching(List(NotImplemented(s"matchPredicateValueSetValue different from IRI: $value")))
    }

  private def predicate2propertyIdValue(predicate: IRI): PropertyIdValue = {
    val (localName, base) = splitIri(predicate)
    val propertyId = new PropertyIdValueImpl(localName, base)
    propertyId
  }

  private def matchPredicateIri(
      predicate: IRI,
      iri: IRI,
      entityDocument: EntityDocument,
      se: WShapeExpr,
      current: EntityDoc
  ): MatchingStatus = {
    val propertyId = predicate2propertyIdValue(predicate)
    entityDocument match {
      case sd: StatementDocument =>
        val statementGroup = sd.findStatementGroup(propertyId)
        if (statementGroup == null) {
          info(s"No statement group for property: $propertyId")
          NoMatching(List(NoStatementGroupProperty(propertyId, entityDocument)))
        } else {
          val statements = statementGroup.getStatements().asScala
          info(s"Statements with predicate $predicate that matched: ${statements}")
          val matched = statements.filter(matchValueStatement(iri))
          info(s"Statements with predicate $predicate that match also value ${iri}: $matched")
          if (matched.isEmpty)
            NoMatching(List(NoStatementMatchesValue(predicate, iri, entityDocument)))
          else
            Matching(List(se), current.mergeStatements(matched.toList))
        }
      case _ =>
        NoMatching(List(NoStatementDocument(entityDocument)))
    }
  }

  private def matchValueStatement(value: IRI)(statement: WDTKStatement): Boolean = {
    val statementValue = statement.getClaim().getValue()
    val valueVisitor: ValueVisitor[Boolean] = MatchVisitor(value)
    if (statementValue == null) {
      false
    } else statementValue.accept(valueVisitor)
  }

  private case class MatchVisitor(expectedIri: IRI) extends ValueVisitor[Boolean] {
    val (localName, base) = splitIri(expectedIri)

    private val expectedEntityId: Option[EntityIdValue] = {
      val itemRegex = """Q(\d+)""".r
      val lexemeRegex = """L(\d+)-S(\d*)""".r
      val propRegex = """P(\d+)""".r
      localName match {
        case itemRegex(_) => Some(new ItemIdValueImpl(localName, base))
        case propRegex(_) => Some(new PropertyIdValueImpl(localName, base))
        case _            => None
      }
    }

    override def visit(v: EntityIdValue): Boolean = expectedEntityId match {
      case None      => false
      case Some(eid) => v == eid
    }

    override def visit(v: GlobeCoordinatesValue): Boolean = false
    override def visit(v: MonolingualTextValue): Boolean = false
    override def visit(v: QuantityValue): Boolean = false
    override def visit(v: WDTKStringValue): Boolean = false
    override def visit(v: TimeValue): Boolean = false
    override def visit(v: UnsupportedValue): Boolean = false
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
