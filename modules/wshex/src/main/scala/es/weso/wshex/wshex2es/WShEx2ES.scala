package es.weso.wshex.es2wshex

import cats._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso._
import es.weso.rbe.interval.{IntLimit, Unbounded}
import es.weso.rdf.nodes._
import es.weso.wbmodel.{Lang => _, Property => _, _}
import es.weso.rbe.interval.IntOrUnbounded
import scala.collection.compat._ // Required for partitionMap
import es.weso.rdf.nodes._
import es.weso.wshex._
import es.weso.wshex.TermConstraint._


case class WShEx2ES(convertOptions: ConvertOptions) extends LazyLogging {

  /** Convert an WShEx schema to an Entity Schema in ShEx
   * 
   */
  def convert(
      schema: WSchema
  ): Either[ES2WShExConvertError, shex.AbstractSchema] = ??? // TODO

}

object WShEx2ES {
  def apply(
      convertOptions: ConvertOptions = ConvertOptions.default
  ): WShEx2ES =
    // Note: 'new' is needed to avoid infinite loop
    new WShEx2ES(convertOptions)

}
