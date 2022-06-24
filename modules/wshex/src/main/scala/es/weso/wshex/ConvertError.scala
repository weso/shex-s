package es.weso.wshex

import es.weso._
import es.weso.rdf.nodes.IRI
import es.weso.shex.implicits.showShEx._
import cats._
import cats.implicits._

sealed abstract class ConvertError extends Exception {
  final override def fillInStackTrace(): Throwable = this
}
case class UnsupportedShapeExpr(se: shex.ShapeExpr, msg: String = "") extends ConvertError {
  override def toString() = s"Unsupported shapeExpr: ${se.show}\n$msg"
}
case class UnsupportedShape(s: shex.Shape, msg: String = "") extends ConvertError {
  override def toString() = s"Unsupported shape: ${s.show}\n$msg"
}
case class UnsupportedNodeConstraint(nc: shex.NodeConstraint) extends ConvertError {
  override def toString() = s"Unsupported nodeConstraint: ${nc.show}"
}
case class UnsupportedValueSetValue(v: shex.ValueSetValue) extends ConvertError {
  override def toString() = s"Unsupported valueSetValue: ${v.show}"
}
case class UnsupportedTripleConstraint(tc: shex.TripleConstraint) extends ConvertError {
  override def toString() = s"Unsupported tripleConstraint: ${tc.show}"
}
case class UnsupportedTripleExpr(te: shex.TripleExpr, msg: String = "") extends ConvertError {
  override def toString() = s"Unsupported tripleExpr: ${te.show}\n$msg"
}
case class CastTripleConstraintError(te: TripleExpr) extends ConvertError {
  override def toString() = s"Cast tripleConstraintError: ${te}"
}
case class UnsupportedPredicate(pred: IRI) extends ConvertError {
  override def toString() = s"Unsupported predicate: ${pred.str}"
}

case class NoValueForPropertyConstraint(n: Int, tc: shex.TripleConstraint) extends ConvertError {
  override def toString() = s"No Value for property constraint P$n\nTripleConstraint:${tc}"
}

case class DifferentPropertyPropertyStatement(n: Int, ns: Int) extends ConvertError {
  override def toString() = s"Different values for proeprty: $n and propertyStatement: $ns"
}

case class NoExprForTripleConstraintProperty(n: Int, s: shex.Shape) extends ConvertError {
  override def toString() = s"TripleConstraint for property: $n, No expression for shape: $s"
}
