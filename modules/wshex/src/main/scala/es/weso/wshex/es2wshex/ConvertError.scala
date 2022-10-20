package es.weso.wshex.es2wshex
import es.weso._
import es.weso.rdf.nodes.IRI
import es.weso.shex.implicits.showShEx._
import cats._
import cats.implicits._
import es.weso.wshex._

sealed abstract class ES2WShExConvertError extends Exception {
  final override def fillInStackTrace(): Throwable = this
}
case class UnsupportedShapeExpr(se: shex.ShapeExpr, msg: String = "") extends ES2WShExConvertError {
  override def toString() = s"Unsupported shapeExpr: ${se.show}\n$msg"
}
case class UnsupportedShape(s: shex.Shape, msg: String = "") extends ES2WShExConvertError {
  override def toString() = s"Unsupported shape: ${s.show}\n$msg"
}
case class UnsupportedNodeConstraint(nc: shex.NodeConstraint) extends ES2WShExConvertError {
  override def toString() = s"Unsupported nodeConstraint: ${nc.show}"
}
case class UnsupportedValueSetValue(v: shex.ValueSetValue) extends ES2WShExConvertError {
  override def toString() = s"Unsupported valueSetValue: ${v.show}"
}
case class UnsupportedTripleConstraint(tc: shex.TripleConstraint) extends ES2WShExConvertError {
  override def toString() = s"Unsupported tripleConstraint: ${tc.show}"
}
case class UnsupportedTripleExpr(te: shex.TripleExpr, msg: String = "") extends ES2WShExConvertError {
  override def toString() = s"Unsupported tripleExpr: ${te.show}\n$msg"
}
case class CastTripleConstraintError(te: shex.TripleExpr) extends ES2WShExConvertError {
  override def toString() = s"Cast tripleConstraintError: ${te}"
}
case class UnsupportedPredicate(pred: IRI, msg: String = "") extends ES2WShExConvertError {
  override def toString() = s"Unsupported predicate: ${pred.str}\n$msg"
}

case class NoValueForPropertyConstraint(n: Int, tc: shex.TripleConstraint) extends ES2WShExConvertError {
  override def toString() = s"No Value for property constraint P$n\nTripleConstraint:${tc}"
}

case class NoValueForPropertyStatementExprs(n: Int, es: List[shex.TripleExpr])
    extends ES2WShExConvertError {
  override def toString() = s"No Value for property statement $n in triple expressions: $es"
}

case class DifferentPropertyPropertyStatement(n: Int, ns: Int, msg: String = "")
    extends ES2WShExConvertError {
  override def toString() = s"Different values for property: $n and propertyStatement: $ns\n$msg"
}

case class NoExprForTripleConstraintProperty(n: Int, s: shex.Shape) extends ES2WShExConvertError {
  override def toString() = s"TripleConstraint for property: $n, No expression for shape: $s"
}

case class UnsupportedExtraProperty(iri: IRI) extends ES2WShExConvertError {
  override def toString() = s"Unsupported EXTRA property: $iri"
}

case class ConvertErrors(es: List[ES2WShExConvertError]) extends ES2WShExConvertError {
  override def toString() =
    s"Conversion errors: ${es.map(_.toString).mkString("\n")}"
}

case class NotFoundShape(ref: shex.ShapeLabel, msg: String) extends ES2WShExConvertError {
  override def toString() = s"Not found shape with Label: $ref\n$msg"
}

case class ErrorConvertingIRI(msg: String) extends ES2WShExConvertError {
  override def toString() = s"Error obtaining EntityId from IRI: $msg"
}

case class UnsupportedShapeExprWasDerivedFrom(n: Int, se: shex.ShapeExpr) extends ES2WShExConvertError {
  override def toString() = s"Unsupported shape expr as value for wasDerivedFrom: $se. Property: $n"
}

