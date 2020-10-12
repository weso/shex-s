package es.weso.shex.validator

import cats.Show
import es.weso.shex.Path

case class ConstraintRef(n: Int, path: Path, showPath: String)  {
  override def toString(): String = s"${showPath}"
}

object ConstraintRef {

  implicit lazy val orderingConstraintRef = new Ordering[ConstraintRef] {

    def compare(c1: ConstraintRef, c2: ConstraintRef): Int = {
      Ordering[Int].compare(c1.n, c2.n)
    }
  }

  implicit lazy val showConstraintRef =
    Show.fromToString[ConstraintRef] 

}
