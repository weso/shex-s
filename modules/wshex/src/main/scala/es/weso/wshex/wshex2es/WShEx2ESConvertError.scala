package es.weso.wshex.wshex2es

import es.weso._
import es.weso.rdf.nodes.IRI
import es.weso.shex.implicits.showShEx._
import cats._
import cats.implicits._
import es.weso.wshex.WNodeKind

sealed abstract class WShEx2ESConvertError extends Exception {
  final override def fillInStackTrace(): Throwable = this

}

case class NotImplementedNodeKind(kind: WNodeKind) extends WShEx2ESConvertError



