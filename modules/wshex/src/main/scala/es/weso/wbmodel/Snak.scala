package es.weso.wbmodel
import org.wikidata.wdtk.datamodel.interfaces.{
  NoValueSnak => WDTKNoValueSnak,
  Snak => WDTKSnak,
  SomeValueSnak => WDTKSomeValueSnak,
  Value => WDTKValue,
  ValueSnak => WDTKValueSnak,
  _
}

sealed abstract trait Snak extends Product with Serializable
object Snak {
  case class ValueSnak(value: Value) extends Snak
  case object NoValueSnak extends Snak
  case object SomeValueSnak extends Snak

  def fromWDTKSnak(snak: WDTKSnak): Snak = {
    val snakVisitor: SnakVisitor[Snak] = ConvertSnakVisitor()
    snak.accept(snakVisitor)
  }

  private case class ConvertSnakVisitor() extends SnakVisitor[Snak] {

    override def visit(v: WDTKValueSnak): Snak = {
      val value = Value.fromWDTKValue(v.getValue())
      Snak.ValueSnak(value)
    }
    override def visit(v: WDTKNoValueSnak): Snak = Snak.NoValueSnak
    override def visit(v: WDTKSomeValueSnak): Snak = Snak.SomeValueSnak
  }

}
