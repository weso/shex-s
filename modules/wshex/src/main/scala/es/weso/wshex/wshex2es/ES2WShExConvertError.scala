package es.weso.wshex.es2wshex

import es.weso._
import es.weso.rdf.nodes.IRI
import es.weso.shex.implicits.showShEx._
import cats._
import cats.implicits._

sealed abstract class ES2WShExConvertError extends Exception {
  final override def fillInStackTrace(): Throwable = this
}
