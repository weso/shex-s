package es.weso.shapepath

import cats.Show
import io.circe.{Encoder, Json}
import cats.syntax.show._
import es.weso.shex.{ShapeExpr, TripleExpr}
import io.circe._
import io.circe.syntax._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._

case class Value(items: List[ShapeNode]) {
  def add(newValue: Value): Value           = Value(items ++ newValue.items)
  def add(s: ShapeExpr): Value              = Value(items ++ List(ShapeExprItem(s)))
  def add(t: TripleExpr): Value             = Value(items ++ List(TripleExprItem(t)))
  def add(newItems: List[ShapeNode]): Value = Value(items ++ newItems)

  def evalChild(nt: NodeTest): Value =
    Value(items.collect { item =>
      item.evalChild(nt) match {
        case Some(i) => i
      }
    })

  def evalNestedShapeExpr(nt: NodeTest): Value =
    Value(items.collect { item =>
      item.evalNestedShapeExpr(nt) match {
        case Some(i) => i
      }
    })

}

object Value {

  /*  implicit lazy val schemaItemEncoder = new Encoder[SchemaItem] {
    final def apply(p: SchemaItem): Json = p.s.asJson
  } */

  implicit lazy val shapeExprItemEncoder: Encoder[ShapeExprItem] = new Encoder[ShapeExprItem] {
    final def apply(p: ShapeExprItem): Json = p.se.asJson
  }
  implicit lazy val tripleExprItemEncoder: Encoder[TripleExprItem] = new Encoder[TripleExprItem] {
    final def apply(p: TripleExprItem): Json = p.te.asJson
  }
  implicit lazy val itemEncoder: Encoder[ShapeNode] = new Encoder[ShapeNode] {
    final def apply(p: ShapeNode): Json = p match {
      // case s : SchemaItem => s.asJson
      case se: ShapeExprItem  => se.asJson
      case te: TripleExprItem => te.asJson
      case IRIItem(iri)       => iri.asJson
    }
  }
  implicit lazy val valueEncoder: Encoder[Value] = new Encoder[Value] {
    final def apply(p: Value): Json = p.items.asJson
  }

  implicit lazy val itemShow: Show[ShapeNode] = new Show[ShapeNode] {
    final def show(i: ShapeNode): String = i match {
      // case s: SchemaItem => s"Schema: ${s.s.show}"
      case se: ShapeExprItem  => s"ShapeExpr: ${se.se.show}"
      case te: TripleExprItem => s"TripleExpr: ${te.te.show}"
      case IRIItem(iri)       => s"IRI: ${iri.show}"
    }
  }

  implicit lazy val valueShow: Show[Value] = new Show[Value] {
    final def show(v: Value): String = s"Value(${v.items.map(_.show).mkString(",")})"
  }

}
