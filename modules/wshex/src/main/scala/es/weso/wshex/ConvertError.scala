package es.weso.wshex

import es.weso._

sealed trait ConvertError extends Throwable
case class UnsupportedShapeExpr(se: shex.ShapeExpr) extends ConvertError
case class UnsupportedShape(s: shex.Shape) extends ConvertError
case class UnsupportedNodeConstraint(nc: shex.NodeConstraint) extends ConvertError
case class UnsupportedValueSetValue(v: shex.ValueSetValue) extends ConvertError
case class UnsupportedTripleConstraint(tc: shex.TripleConstraint) extends ConvertError
case class UnsupportedTripleExpr(te: shex.TripleExpr) extends ConvertError
case class CastTripleConstraintError(te: TripleExpr) extends ConvertError
