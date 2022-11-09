package es.weso.wbmodel
import org.wikidata.wdtk.datamodel.interfaces.{Snak => WDTKSnak, SnakGroup => WDTKSnakGroup}
import org.wikidata.wdtk.datamodel.implementation.SnakGroupImpl
import org.wikidata.wdtk.datamodel.implementation.SnakImpl
import org.wikidata.wdtk.datamodel.implementation.NoValueSnakImpl
import org.wikidata.wdtk.datamodel.implementation.ValueSnakImpl
import org.wikidata.wdtk.datamodel.implementation.SomeValueSnakImpl
import org.wikidata.wdtk.datamodel.interfaces.PropertyIdValue
import collection.JavaConverters._
import es.weso.wbmodel.Snak.NoValueSnak
import es.weso.wbmodel.Snak.SomeValueSnak
import es.weso.wbmodel.Snak.ValueSnak

case class Qualifiers(qsMap: Map[PropertyId, List[Snak]]) {

  lazy val isEmpty = qsMap.isEmpty

  def withSnak(property: PropertyId, snak: Snak): Qualifiers =
    Qualifiers(qsMap.updated(property, snak +: qsMap.getOrElse(property, List())))

  def getSnaks(): List[Snak] =
    qsMap.values.flatten.toList

  private def mkSnak(s: Snak, p: PropertyIdValue): WDTKSnak = s match {
    case _: NoValueSnak   => new NoValueSnakImpl(p)
    case _: SomeValueSnak => new SomeValueSnakImpl(p)
    case v: ValueSnak =>
      v.value.wdtkValue match {
        case None =>
          throw new RuntimeException(
            s"Converting Snak to value without WDTK value: $s for property $p"
          )
        case Some(v) => new ValueSnakImpl(p, v)
      }
  }

  lazy val snakGroups: List[WDTKSnakGroup] =
    qsMap.map { case (p, snaks) =>
      new SnakGroupImpl(snaks.map(mkSnak(_, p.toWDTKValue)).asJava)
    }.toList
}

object Qualifiers {

  def empty: Qualifiers = Qualifiers(Map())

  def fromSnakGroups(sgs: java.util.List[WDTKSnakGroup]): Qualifiers = {
    val m = sgs.asScala.map { case sg =>
      (
        PropertyId.fromPropertyIdValue(sg.getProperty()),
        sg.getSnaks().asScala.toList.map(Snak.fromWDTKSnak(_))
      )
    }.toMap
    Qualifiers(m)
  }

  def fromSnaks(snaks: List[Snak]): Qualifiers =
    Qualifiers(snaks.groupBy(_.propertyId))
}
