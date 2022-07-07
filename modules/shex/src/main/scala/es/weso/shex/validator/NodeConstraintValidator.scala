package es.weso.shex.validator

import cats._
import implicits._
import cats.effect.IO
import es.weso.shex._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rbe.interval.IntervalChecker
import es.weso.rbe.Empty
import es.weso.utils.SetUtils
import es.weso.shex.implicits.showShEx._
import es.weso.shapemaps.{
  BNodeLabel => BNodeMapLabel,
  IRILabel => IRIMapLabel,
  Start => StartMapLabel,
  _
}
import es.weso.shex.actions.TestSemanticAction
import Function.tupled
import es.weso.shex.validator.ShExError._
import es.weso.shex.validator.ConstraintRef.{showConstraintRef => _}
import es.weso.utils.internal.CollectionCompat._
import es.weso.utils.VerboseLevel

/** Node Constraint validator
  */
case class NodeConstraintValidator(schema: ResolvedSchema) extends ShExChecker {

  def checkNodeConstraint(attempt: Attempt, node: RDFNode, s: NodeConstraint): CheckTyping =
    (for {
      t1 <- optCheck(s.nodeKind, checkNodeKind(attempt, node), getTyping)
      t2 <- optCheck(s.values, checkValues(attempt, node), getTyping)
      t3 <- optCheck(s.datatype, checkDatatype(attempt, node), getTyping)
      t4 <- checkXsFacets(attempt, node)(s.xsFacets)
      t <- combineTypings(List(t1, t2, t3, t4))
    } yield t).handleErrorWith(e =>
      debug(s"Error on checkNodeConstraint(${node.show}@${s.show} failed: ${e.msg}") *>
        err(e)
    )

  private def checkNodeKind(attempt: Attempt, node: RDFNode)(nk: NodeKind): CheckTyping =
    nk match {
      case IRIKind =>
        checkCond(
          node.isIRI,
          attempt,
          StringError(s"${node.show} is not an IRI"),
          s"${node.show} is an IRI"
        )
      case BNodeKind =>
        checkCond(
          node.isBNode,
          attempt,
          StringError(s"${node.show} is not a BlankNode"),
          s"${node.show} is a BlankNode"
        )
      case NonLiteralKind =>
        checkCond(
          !node.isLiteral,
          attempt,
          StringError(s"${node.show} is a literal but should be a NonLiteral"),
          s"${node.show} is NonLiteral"
        )
      case LiteralKind =>
        checkCond(
          node.isLiteral,
          attempt,
          StringError(s"${node.show} is not an Literal"),
          s"${node.show} is a Literal"
        )
    }

  private def checkXsFacets(attempt: Attempt, node: RDFNode)(xsFacets: List[XsFacet]): CheckTyping =
    if (xsFacets.isEmpty) getTyping
    else getRDF.flatMap(rdf => FacetChecker(schema, rdf).checkFacets(attempt, node)(xsFacets))

  private def checkValueSetValue(attempt: Attempt, node: RDFNode)(v: ValueSetValue): CheckTyping =
    ValueChecker(schema).checkValue(attempt, node, v)

  private def checkValues(attempt: Attempt, node: RDFNode)(
      values: List[ValueSetValue]
  ): CheckTyping = {
    val cs: List[CheckTyping] = values.map(checkValueSetValue(attempt, node))
    checkSome(
      cs,
      StringError(s"${node.show} does not belong to [${values.map(_.show).mkString(",")}]")
    )
  }

  private def checkDatatype(attempt: Attempt, node: RDFNode)(datatype: IRI): CheckTyping =
    getRDF.flatMap(rdf =>
      fromIO(rdf.checkDatatype(node, datatype)).flatMap(hasDatatype =>
        checkCond(
          hasDatatype,
          attempt,
          CheckDatatypeError(node, datatype, rdf),
          s"${node.show} has datatype ${datatype.show}"
        )
      )
    )

}
