package es.weso.wshex.matcher
import es.weso.wshex._
import es.weso.wshex.es2wshex._

sealed abstract class MatcherParseError(msg: String) extends Product with Serializable
case class ParseException(e: Throwable) extends MatcherParseError(e.getMessage())
case class ConversionError(e: ES2WShExConvertError)
    extends MatcherParseError(s"Error converting shEx to WShEx\nError: ${e}")
