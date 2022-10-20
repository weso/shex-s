package es.weso.wshex.matcher
import es.weso.wshex._
import es.weso.wshex.es2wshex._

sealed abstract class ParseError(msg: String) extends Product with Serializable
case class ParseException(e: Throwable) extends ParseError(e.getMessage())
case class ConversionError(e: ES2WShExConvertError)
    extends ParseError(s"Error converting shEx to WShEx\nError: ${e}")
