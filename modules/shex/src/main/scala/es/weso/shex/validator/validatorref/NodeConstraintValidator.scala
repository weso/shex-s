package es.weso.shex.validator.validatorref

import es.weso.shex.NodeConstraint
import cats.effect.IO
import es.weso.shex._
import es.weso.rdf.nodes._
import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._

object NodeConstraintValidator {
  def validateNodeConstraint(
      node: RDFNode,
      nc: NodeConstraint
  ): ValidatedNel[NodeConstraintError, List[NodeConstraintEvidence]] = {
    combineVs(
      optValidate(nc.nodeKind, node, nodeKind),
      optValidate(nc.datatype, node, datatype),
      optValidate(nc.values, node, values)
    ).combine(
      validateList(nc.xsFacets, node, facet)
    )
  }

  private def nodeKind(nk: NodeKind, node: RDFNode): ValidatedNel[NodeConstraintError, NodeConstraintEvidence] =
    nk match {
      case IRIKind =>
        if (node.isIRI) IRIKindEvidence(node).valid
        else IRIKindError(node).invalidNel
      case BNodeKind =>
        if (node.isBNode) BNodeKindEvidence(node).valid
        else BNodeKindError(node).invalidNel
      case NonLiteralKind =>
        if (!node.isLiteral) NonLiteralKindEvidence(node).valid
        else NonLiteralKindError(node).invalidNel
      case LiteralKind =>
        if (node.isLiteral) LiteralKindEvidence(node).valid
        else LiteralKindError(node).invalidNel
    }

  private def datatype(dt: IRI, node: RDFNode): ValidatedNel[NodeConstraintError, NodeConstraintEvidence] =
    node match {
      case literal: Literal =>
        if (literal.dataType == dt) DatatypeEvidence(node, dt).valid
        else LiteralNonDatatypeError(node, literal.dataType, dt).invalidNel
      case _ => DatatypeErrorNonLiteral(node, dt).invalidNel
    }

  private def values(
      vs: List[ValueSetValue],
      node: RDFNode
  ): ValidatedNel[NodeConstraintError, NodeConstraintEvidence] =
    NotImplementedNodeConstraintError(node, s"ValueSet ${vs}").invalidNel

  private def facet(f: XsFacet, node: RDFNode): ValidatedNel[NodeConstraintError, NodeConstraintEvidence] =
    NotImplementedNodeConstraintError(node, s"facet ${f}").invalidNel

  private def optValidate[A, B, V, E](
      optValue: Option[A],
      value: V,
      validator: (A, V) => ValidatedNel[E, B]
  ): ValidatedNel[E, Option[B]] =
    optValue.fold(valid(None))(c => validator(c, value).map(Some(_)))

  private def validateList[A, B, E, V](
      vs: List[A],
      value: V,
      validator: (A, V) => ValidatedNel[E, B]
  ): ValidatedNel[E, List[B]] = {
    vs.map(validator(_, value)).sequence
  }

  private def combineVs[E, A](vs: ValidatedNel[E, Option[A]]*): ValidatedNel[E, List[A]] =
    vs.map(_.map(_.toList)).combineAll
}
