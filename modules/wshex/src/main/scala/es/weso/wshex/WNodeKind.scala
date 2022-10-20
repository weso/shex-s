package es.weso.wshex

import org.wikidata.wdtk.datamodel.interfaces.MonolingualTextValue
import es.weso.wbmodel._
import cats.implicits._

// See https://www.mediawiki.org/w/index.php?title=Wikibase/DataModel#Datatypes_and_their_Values
sealed abstract class WNodeKind {
  def matchSnak(snak: Snak): Either[Reason, Unit] = snak match {
    case valueSnak: Snak.ValueSnak => matchValue(valueSnak.value)
    case Snak.NoValueSnak => this match {
      case WNodeKind.NoValueKind => ().asRight
      case _ => WnodeKindMatchError_NoValue(snak).asLeft
    }
    case Snak.SomeValueSnak => this match {
      case WNodeKind.SomeValueKind => ().asRight
      case _ => WnodeKindMatchError_SomeValue(snak).asLeft
    }
  }
  def matchValue(value: Value): Either[Reason, Unit] = this match {
    case WNodeKind.NoValueKind => WnodeKindMatchError_NoValue(Snak.ValueSnak(value)).asLeft
    case WNodeKind.SomeValueKind => WnodeKindMatchError_SomeValue(Snak.ValueSnak(value)).asLeft
    case WNodeKind.LiteralKind => value match {
        case _: StringValue | 
             _: DateValue => ().asRight
        case _ => NotImplemented(s"LiteralKind. Failed for value: $value").asLeft
      }
    case WNodeKind.StringKind => value match {
        case _: StringValue => ().asRight
        case _ => NoStringDatatype(value).asLeft
      }
    case WNodeKind.QuantityKind   => value match {
        case _: QuantityValue => ().asRight
        case _ => NoStringDatatype(value).asLeft
      }
    case WNodeKind.TimeKind => value match {
        case _: DateValue => ().asRight
        case _            => NoDateDatatype(value).asLeft
      }
    case _ => NotImplemented(s"matchKind. Not implemented yet: $this for value: $value").asLeft
  }
}
object WNodeKind {
  // Should we remove these kinds and represent them as datatype IRIs ?
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
  case object ValueKind extends WNodeKind 
  case object SomeValueKind extends WNodeKind 
  case object NoValueKind extends WNodeKind 
}