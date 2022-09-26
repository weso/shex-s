package es.weso.wshex

import org.wikidata.wdtk.datamodel.interfaces.MonolingualTextValue

// See https://www.mediawiki.org/w/index.php?title=Wikibase/DataModel#Datatypes_and_their_Values
sealed abstract class WNodeKind
object WNodeKind {
  case object LiteralKind extends WNodeKind
  case object TimeKind extends WNodeKind
  case object QuantityKind extends WNodeKind
  case object StringKind extends WNodeKind
  case object MonolingualTextKind extends WNodeKind
  case object MultilingualTextKind extends WNodeKind
  case object GeoCoodrinatesKind extends WNodeKind
  case object GeoShapeKind extends WNodeKind
  case object MediaKind extends WNodeKind
  case object IriKind extends WNodeKind
}