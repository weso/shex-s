package es.weso.shex.validator

// import cats._
// import cats.implicits._
import es.weso.collection.Bag
import es.weso.rdf.nodes.RDFNode
import es.weso.shex.{SemAct, ShapeExpr}
import es.weso.utils.SeqUtils.filterOptions
import ConstraintRef._
import io.circe.Json

case class CandidateLine(values: List[(Arc,ConstraintRef)]) {
  def mkBag: Bag[ConstraintRef] = Bag.toBag(values.map(_._2))

  def nodeConstraints(table: CTable): List[(RDFNode, (ShapeExpr, Option[List[SemAct]]))] =
    filterOptions(values.map {
      case (arc, cref) => (arc.node, table.getConstraint(cref))
    })

//  override def toString: String = CandidateLine.showCandidateLine.show(this)

def toJson: Json = 
  Json.fromValues(values.map(pair2Json(_)))

def pair2Json(pair: (Arc,ConstraintRef)): Json = {
  val (arc,c) = pair
  Json.obj(
   ("type", Json.fromString("CandidateLine")),
   ("arc", Json.fromString(arc.toString)),
   ("constraint", Json.fromString(c.toString))
  )
 }

}

object CandidateLine {

/*  implicit lazy val showCandidateLine = new Show[CandidateLine] {
    def show(cl: CandidateLine): String = {
      def compare(pair1:(Arc,ConstraintRef), pair2:(Arc,ConstraintRef)): Boolean =
        Ordering[ConstraintRef].compare(pair1._2, pair2._2) <= 0

      s"Candidate line:\n${cl.values.sortWith(compare).map{ case (arc,cref) => s"${arc.show} as ${cref.show}"}.mkString("\n")}"
    }
  }
*/

 

}
