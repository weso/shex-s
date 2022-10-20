package es.weso.wshex
import cats.implicits._

sealed trait WShExFormat {
  def name: String
}
object WShExFormat {
  case object CompactWShExFormat extends WShExFormat {
    override def name = "WShExC"
  }
  case object JsonWShExFormat extends WShExFormat {
    override def name = "WShExJson"
  }
  case object ESCompactFormat extends WShExFormat {
    override def name = "ESCompact"
  }
  case object ESJsonFormat extends WShExFormat {
    override def name = "ESJson"
  }

  val availableFormats: List[WShExFormat] = 
    List(CompactWShExFormat, JsonWShExFormat, ESCompactFormat, ESJsonFormat)

  def fromString(str: String): Either[ConvertWShExFormatError, WShExFormat] = {
    val strUpper = str.toUpperCase()
    availableFormats.collectFirst { 
      case f if f.name.toUpperCase() == strUpper => f
    }.fold(
      ConvertWShExFormatError(str).asLeft[WShExFormat]
      )(
      _.asRight[ConvertWShExFormatError]
      )
  }

  case class ConvertWShExFormatError(str: String) 
   extends RuntimeException(s"Error converting $str to WShExFormat")  
}
