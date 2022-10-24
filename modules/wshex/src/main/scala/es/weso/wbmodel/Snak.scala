package es.weso.wbmodel
import org.wikidata.wdtk.datamodel.interfaces.{
    Snak => WDTKSnak,
    ValueSnak => WDTKValueSnak,
    NoValueSnak => WDTKNoValueSnak,
    SomeValueSnak => WDTKSomeValueSnak,
    Value => WDTKValue,
    _
}
import cats.implicits._
import org.wikidata.wdtk.datamodel.implementation.{ValueSnakImpl, NoValueSnakImpl, SomeValueSnakImpl}

sealed abstract trait Snak extends Product with Serializable {
  def propertyId: PropertyId
  def wdtkSnak: Option[WDTKSnak]

  def toWDTKSnak: WDTKSnak
}

object Snak {

 case class ValueSnak(
   value: Value, 
   propertyId: PropertyId, 
   wdtkSnak: Option[WDTKSnak] = None
   ) extends Snak {
  
  override def toWDTKSnak: WDTKSnak = 
    wdtkSnak match {
      case None => new ValueSnakImpl(propertyId.toWDTKValue, value.toWDTKValue)
      case Some(v) => v
    }
}
 
 case class NoValueSnak(
   propertyId: PropertyId, 
   wdtkSnak: Option[WDTKSnak] = None
   ) extends Snak {

  override def toWDTKSnak: WDTKSnak = 
    wdtkSnak match {
      case None => new NoValueSnakImpl(propertyId.toWDTKValue)
      case Some(v) => v
    }

   }

 case class SomeValueSnak(
  propertyId: PropertyId, 
  wdtkSnak: Option[WDTKSnak] = None
  ) extends Snak {

  override def toWDTKSnak: WDTKSnak = 
    wdtkSnak match {
      case None => new SomeValueSnakImpl(propertyId.toWDTKValue)
      case Some(v) => v
    }

  }

 def fromWDTKSnak(snak: WDTKSnak): Snak = {
    val snakVisitor: SnakVisitor[Snak] = ConvertSnakVisitor()
    snak.accept(snakVisitor)
 }

 private case class ConvertSnakVisitor() extends SnakVisitor[Snak] {

    override def visit(v: WDTKValueSnak): Snak = {
        val value = Value.fromWDTKValue(v.getValue())
        val property = PropertyId.fromPropertyIdValue(v.getPropertyId())
        Snak.ValueSnak(value, property, v.some)
    }
    override def visit(v: WDTKNoValueSnak): Snak = {
      val property = PropertyId.fromPropertyIdValue(v.getPropertyId())
      Snak.NoValueSnak(property, v.some) 
    }
    override def visit(v: WDTKSomeValueSnak): Snak = { 
      val property = PropertyId.fromPropertyIdValue(v.getPropertyId())
      Snak.SomeValueSnak(property, v.some)
    }
  }

}