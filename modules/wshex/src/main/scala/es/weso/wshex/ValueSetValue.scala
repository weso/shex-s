package es.weso.wshex

import es.weso.wbmodel._
import es.weso.rdf.nodes._
import cats.implicits._

sealed trait ValueSetValue {
    def matchValue(value: Value): Either[NoMatchValueSetValue, Unit]
}
sealed trait NonLocalValueSetValue extends ValueSetValue {
    def matchValue(value: Value): Either[NoMatchValueSetValue, Unit] = 
        NoMatchValueSetValue_NonLocal(value,this).asLeft
}
sealed trait LocalValueSetValue extends ValueSetValue 

case class EntityIdValueSetValue(id: EntityId) extends LocalValueSetValue {
    override def matchValue(value: Value): Either[NoMatchValueSetValue, Unit] = 
      value match {
        case eid: EntityId => if (eid.iri == id.iri) ().asRight 
          else NoMatchValueSetValue_EntityIdDifferent(value, this).asLeft
        case e: Entity => if (e.entityId == id)   ().asRight
          else NoMatchValueSetValue_EntityIdDifferent(value, this).asLeft  
        case _ => NoMatchValueSetValue_NotImplemented(value, this).asLeft  
      }
}
case class IRIValueSetValue(iri: IRI) extends LocalValueSetValue {
    override def matchValue(value: Value): Either[NoMatchValueSetValue, Unit] = 
      NoMatchValueSetValue_NotImplemented(value, this).asLeft        
}
case class StringValueSetValue(str: String) extends LocalValueSetValue {
    override def matchValue(value: Value): Either[NoMatchValueSetValue, Unit] = 
      NoMatchValueSetValue_NotImplemented(value, this).asLeft        
}

case class IRIStem(stem: IRI) extends LocalValueSetValue {
  override def matchValue(value: Value): Either[NoMatchValueSetValue, Unit] = 
    value match {
      case eid: EntityId => 
        if (eid.iri.getLexicalForm.startsWith(stem.getLexicalForm)) ().asRight
        else NoMatchValueSetValue_IRIStem(value, this).asLeft
      case e: Entity => 
        if (e.entityId.iri.getLexicalForm.startsWith(stem.getLexicalForm)) ().asRight
        else NoMatchValueSetValue_IRIStem(value, this).asLeft
      case _ => NoMatchValueSetValue_IRIStem(value, this).asLeft
    }
}