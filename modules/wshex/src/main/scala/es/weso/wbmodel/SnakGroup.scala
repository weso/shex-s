package es.weso.wbmodel
import collection.JavaConverters._
import org.wikidata.wdtk.datamodel.interfaces.{SnakGroup => WDTKSnakGroup}
import org.wikidata.wdtk.datamodel.implementation.SnakGroupImpl
import cats.implicits._

case class SnakGroup(
    snaks: List[Snak],
    sg: Option[WDTKSnakGroup] = None
) {
  lazy val toWDTKSnakGroup: WDTKSnakGroup = sg match {
    case None    => new SnakGroupImpl(snaks.map(_.toWDTKSnak).asJava)
    case Some(v) => v
  }
}

object SnakGroup {

  def fromWDTKSnakGroup(sg: WDTKSnakGroup): SnakGroup =
    SnakGroup(
      sg.getSnaks().asScala.toList.map(Snak.fromWDTKSnak(_)),
      sg.some
    )

  def mkSnakGroups(snaks: List[Snak]): List[SnakGroup] =
    snaks
      .groupBy(_.propertyId)
      .toList
      .map(_._2)
      .map(SnakGroup(_))
}
