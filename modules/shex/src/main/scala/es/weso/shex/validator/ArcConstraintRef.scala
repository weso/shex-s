package es.weso.shex.validator

import cats._
import cats.implicits._
import es.weso.rdf.nodes.RDFNode
import es.weso.shex.Path
import io.circe.Json

case class ArcConstraintRef(arc: Arc, cRef: ConstraintRef) {

  def toJson: Json = Json.obj(
    ("type", Json.fromString("CandidateLine")),
    ("arc", Json.fromString(arc.toString)),
    ("constraint", Json.fromString(cRef.toString))
  )

}

object ArcConstraintRef {

  implicit lazy val showArcConstraintRef: Show[ArcConstraintRef] = new Show[ArcConstraintRef] {
    override def show(c: ArcConstraintRef): String =
      s"${c.arc.show} ~ ${c.cRef.show}"
  }

}
