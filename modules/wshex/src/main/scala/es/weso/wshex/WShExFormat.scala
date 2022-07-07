package es.weso.wshex

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
}
