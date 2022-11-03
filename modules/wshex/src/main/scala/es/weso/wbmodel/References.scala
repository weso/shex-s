package es.weso.wbmodel
import collection.JavaConverters._
import org.wikidata.wdtk.datamodel.interfaces.{Reference => WDTKReference}

case class References(refs: List[Reference]) {
  def isEmpty: Boolean = refs.isEmpty

  def withReference(ref: Reference): References =
    References(ref +: refs)

  lazy val asWDTKReferences: List[WDTKReference] =
    refs.map(_.toWDTKReference)
}

object References {

  def empty: References = References(List())

  def fromWDTKReferences(ls: java.util.List[WDTKReference]): References =
    References(ls.asScala.toList.map(Reference.fromWDTKReference(_)))
}
