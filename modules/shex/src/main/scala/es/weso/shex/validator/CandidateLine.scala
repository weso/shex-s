package es.weso.shex.validator

// import cats._
// import cats.implicits._
import es.weso.collection.Bag
import es.weso.rdf.nodes.RDFNode
import es.weso.shex.{SemAct, ShapeExpr}
import es.weso.utils.SeqUtils.filterOptions
import ConstraintRef._
import io.circe.Json

case class CandidateLine(values: List[ArcConstraintRef]) {
  def mkBag: Bag[ConstraintRef] = Bag.toBag(values.map(_.cRef))

  def nodeConstraints(table: CTable): List[(RDFNode, (ShapeExpr, Option[List[SemAct]]))] =
    filterOptions(values.map { ac =>
      (ac.arc.node, table.getConstraint(ac.cRef))
    })

//  override def toString: String = CandidateLine.showCandidateLine.show(this)

  def toJson: Json =
    Json.fromValues(values.map(_.toJson))

}

object CandidateLine {}
