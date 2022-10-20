package es.weso.wbmodel
import org.wikidata.wdtk.datamodel.interfaces.{ 
    SnakGroup => WDTKSnakGroup
} 

case class Qualifiers(qsmap: Map[PropertyId, List[Snak]]) extends AnyVal {
  def getSnakGroups(): List[WDTKSnakGroup] = {
    ???
  }
}