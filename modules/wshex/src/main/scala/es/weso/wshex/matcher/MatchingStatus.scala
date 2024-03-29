package es.weso.wshex.matcher

import es.weso.rdf.nodes.IRI
import cats._
import cats.data._
import es.weso.utils.internal.CollectionCompat._
import es.weso.wshex._
import es.weso.wbmodel.EntityDoc
// import es.weso.wbmodel._

abstract class MatchingStatus {
  def matches: Boolean
  def dependencies: List[Dependency]
  def and(other: => MatchingStatus): MatchingStatus

  /**
    * Defines a disjunction between one matching status and another
    * 
    * @param other the other matching status
    * @param mergeOrs if true, it merges the values of the matchings when both conform (it can have a bad performance, but can be useful to generate subsets)
    * @return
    */
  def or(other: => MatchingStatus, mergeOrs: Boolean): MatchingStatus
}

/** Represents a valid matching of an entity with a list of shape expressions
  */
case class Matching(
    shapeExprs: List[WShapeExpr],
    entity: EntityDoc, // Matched entity
    override val dependencies: List[Dependency] = List()
) extends MatchingStatus {

  override def matches: Boolean = true
  override def and(other: => MatchingStatus): MatchingStatus = other match {
    case m: Matching =>
      Matching(
        shapeExprs = this.shapeExprs ++ m.shapeExprs,
        entity = merge(this.entity, m.entity),
        this.dependencies ++ m.dependencies
      )
    case nm: NoMatching => nm
  }

  override def or(other: => MatchingStatus, mergeOrs: Boolean): MatchingStatus = 
    if (!mergeOrs) this 
    else other match {
    case ms: Matching => Matching(
      shapeExprs = this.shapeExprs ++ ms.shapeExprs,
      entity = merge(this.entity, ms.entity),
      dependencies = this.dependencies ++ ms.dependencies
    )
    case _ => this
  }

  private def merge(e: EntityDoc, other: EntityDoc): EntityDoc = e.merge(other)

  def withDependencies(ds: List[Dependency]) = this.copy(dependencies = ds)

}

case class NoMatching(
    matchingErrors: List[MatchingError],
    override val dependencies: List[Dependency] = List()
) extends MatchingStatus {
  override def matches: Boolean = false

  override def and(other: => MatchingStatus): MatchingStatus = this
  override def or(other: => MatchingStatus, mergeOrs: Boolean): MatchingStatus =  
    other match {
     case m: Matching => m
     case nm: NoMatching =>
      NoMatching(
        matchingErrors = this.matchingErrors ++ nm.matchingErrors,
        dependencies = this.dependencies ++ nm.dependencies
      )
  }

}

object MatchingStatus {

  def matchEmpty(e: EntityDoc): MatchingStatus =
    Matching(shapeExprs = List(), entity = e)

  lazy val noMatchingEmpty: MatchingStatus =
    NoMatching(matchingErrors = List())

  def combineAnds(e: EntityDoc, ls: LazyList[MatchingStatus]): MatchingStatus =
    ls.foldLeft(matchEmpty(e))(_.and(_))

  def combineOrs(ls: LazyList[MatchingStatus], mergeOrs: Boolean): MatchingStatus =
    ls.foldLeft(noMatchingEmpty)(_.or(_, mergeOrs))

}
