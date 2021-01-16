package es.weso.shapepath

import cats.Show
import io.circe.{Encoder, Json}
import cats.syntax.show._
import es.weso.shex.{ShapeExpr, TripleExpr}
import io.circe._
import io.circe.syntax._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._


case class Value(items: List[Item]) {
  def add(newValue: Value): Value = Value(items ++ newValue.items)
  def add(s: ShapeExpr): Value = Value(items ++ List(ShapeExprItem(s)))
  def add(t: TripleExpr): Value = Value(items ++ List(TripleExprItem(t)))
  def add(newItems: List[Item]): Value = Value(items ++ newItems)
}

object Value {

/*  implicit lazy val schemaItemEncoder = new Encoder[SchemaItem] {
    final def apply(p: SchemaItem): Json = p.s.asJson
  } */

  implicit lazy val shapeExprItemEncoder = new Encoder[ShapeExprItem] {
    final def apply(p: ShapeExprItem): Json = p.se.asJson
  }
  implicit lazy val tripleExprItemEncoder = new Encoder[TripleExprItem] {
    final def apply(p: TripleExprItem): Json = p.te.asJson
  }
  implicit lazy val itemEncoder = new Encoder[Item] {
    final def apply(p: Item): Json = p match {
      // case s : SchemaItem => s.asJson
      case se: ShapeExprItem => se.asJson
      case te: TripleExprItem => te.asJson
    }
  }
  implicit lazy val valueEncoder = new Encoder[Value] {
    final def apply(p: Value): Json = p.items.asJson
  }

  implicit lazy val itemShow = new Show[Item] {
    final def show(i: Item): String = i match {
      // case s: SchemaItem => s"Schema: ${s.s.show}"
      case se: ShapeExprItem => s"ShapeExpr: ${se.se.show}"
      case te: TripleExprItem => s"TripleExpr: ${te.te.show}"
    }
  }

  implicit lazy val valueShow = new Show[Value] {
    final def show(v: Value): String = s"Value(${v.items.map(_.show).mkString(",")})"
  }

}
