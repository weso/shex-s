package es.weso.shex

import cats.Show

case class VarName(str: String) extends AnyVal

object VarName {
  implicit lazy val showCandidateLine: Show[VarName] = new Show[VarName] {
    def show(cl: VarName): String = cl.str
  }

}
