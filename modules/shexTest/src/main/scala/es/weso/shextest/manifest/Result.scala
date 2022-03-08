package es.weso.shextest.manifest

import es.weso.rdf.nodes._
import scala.concurrent.duration.FiniteDuration

case class Result(name: String, isOk: Boolean, reason: Reason, time: Option[FiniteDuration] = None) {
  def withTime(time: FiniteDuration): Result = this.copy(time = Some(time))
}

sealed trait ResultExpected {

  def asBoolean: Option[Boolean] = {
    this match {
      case BooleanResult(b) => Some(b)
      case _                => None
    }
  }

  val isValid: Boolean

  def resultShapeMap: Option[IRI] = this match {
    case ResultShapeMapIRI(iri) => Some(iri)
    case _                      => None
  }
}

final case class ResultShapeMapIRI(iri: IRI) extends ResultExpected {
  override val isValid = false
}

case class ValidPair(node: RDFNode, shape: RDFNode)

final case class BooleanResult(value: Boolean) extends ResultExpected {
  override val isValid = value
}

final case class IRIResult(value: IRI) extends ResultExpected {
  override val isValid = false
}

case object EmptyResult extends ResultExpected {
  override val isValid = true
}
