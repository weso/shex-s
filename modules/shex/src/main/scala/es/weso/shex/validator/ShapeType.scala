package es.weso.shex.validator
import cats._
import es.weso.shex.{AbstractSchema, Schema, ShapeDecl, ShapeExpr, ShapeLabel}
import io.circe._
import io.circe.syntax._
import es.weso.rdf.locations.Location

case class ShapeType(se: ShapeExpr, label: Option[ShapeLabel], schema: AbstractSchema) {
  def hasLabel(expectedLabel: ShapeLabel): Boolean =
    label.fold(false)(_ == expectedLabel)

  def isAbstract: Boolean = se match {
    case _: ShapeDecl => true
    case _            => false
  }
}

object ShapeType {

  def apply(shape: ShapeExpr, schema: Schema): ShapeType = ShapeType(shape, None, schema)

  implicit lazy val showShapeType: Show[ShapeType] = new Show[ShapeType] {
    override def show(s: ShapeType) =
      s.label match {
        case None      => s"Anonymous shape" // Show[ShapeExpr].show(s.shape)
        case Some(lbl) => s"${s.schema.qualify(lbl)}"
      }
  }

  private def optAdd[A](opt: Option[A], obj: JsonObject, label: String, f: A => Json) =
    opt match {
      case None    => obj
      case Some(x) =>
        // println(s"Adding label: ${label} to ${obj}")
        obj.add(label, f(x))
    }

  // TODO: Move this to Location object
  implicit val locationEncoder: Encoder[Location] = new Encoder[Location] {

    final def apply(loc: Location): Json =
      Json.fromFields(
        List(
          ("line", loc.line.asJson),
          ("col", loc.col.asJson),
          ("type", loc.tokenType.asJson)
        ) ++ (loc.source match {
          case None      => List()
          case Some(iri) => List(("source", iri.str.asJson))
        })
      )
  }

  implicit val shapeTypeEncoder: Encoder[ShapeType] = new Encoder[ShapeType] {
    final def apply(v: ShapeType): Json = {
      // println(s"@@@@@@###### ShapeTypeEncoder!!: ${v}")
      val obj = JsonObject(("type", "Shape".asJson))
      val extended = v.label match {
        case None      => obj
        case Some(lbl) =>
          // println(s"@@@ Label: ${lbl}. LocationMap: ${v.schema.labelLocationMap} " )
          optAdd(
            v.schema.labelLocationMap.map(_.get(lbl)).flatten,
            obj.add("label", lbl.toRDFNode.getLexicalForm.asJson),
            "location",
            (loc: Location) => loc.asJson
          )
      }
      Json.fromJsonObject(extended)
    }

  }

}
