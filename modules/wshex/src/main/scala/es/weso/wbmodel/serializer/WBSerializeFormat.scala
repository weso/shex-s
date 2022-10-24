package es.weso.wbmodel.serializer

import es.weso.utils.named._

sealed abstract class WBSerializeFormat extends Named {
  def sep: String
}

object WBSerializeFormat {

  case object Turtle extends WBSerializeFormat {
    override val name: String = "Turtle"
    override val sep: String  = "\n"
  }

  case object JSON extends WBSerializeFormat {
    override val name: String = "JSON"
    override val sep: String  = ",\n"
  }

  case object Plain extends WBSerializeFormat {
    override val name: String = "Text"
    override val sep: String  = "\n"
  }

  lazy val availableFormats = List(Turtle, JSON, Plain)

}
