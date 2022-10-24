package es.weso.wbmodel
import org.wikidata.wdtk.datamodel.interfaces.{Reference => WDTKReference}
import org.wikidata.wdtk.datamodel.implementation.ReferenceImpl
import collection.JavaConverters._
import cats.implicits._


case class Reference(
    groups: List[SnakGroup], 
    wdtkReference: Option[WDTKReference] = None
    ) {
    def getSnaks(): List[Snak] = 
        groups.map(_.snaks).flatten

    lazy val toWDTKReference: WDTKReference = wdtkReference.fold(
      ReferenceImpl(groups.map(_.toWDTKSnakGroup).asJava)
    )(identity)
}


object Reference {
    def fromWDTKReference(reference: WDTKReference): Reference = {
        Reference(
         reference.getSnakGroups().asScala.toList.map(SnakGroup.fromWDTKSnakGroup(_)),
         reference.some
        )    
    }

    def fromSnaks(snaks: List[Snak]): Reference = 
        Reference(mkSnakGroups(snaks))

    private def mkSnakGroups(snaks: List[Snak]): List[SnakGroup] = 
        snaks
        .groupBy(_.propertyId)
        .toList
        .map(_._2)
        .map(SnakGroup(_))

}