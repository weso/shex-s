package es.weso.wbmodel

import org.wikidata.wdtk.datamodel.interfaces.DatatypeIdValue

case class Datatype(name: String) extends AnyVal

object Datatype {
  lazy val defaultDatatype = Datatype(DatatypeIdValue.DT_ITEM)
}
