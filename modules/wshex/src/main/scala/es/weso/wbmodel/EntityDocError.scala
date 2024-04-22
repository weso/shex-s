package es.weso.wbmodel
import org.wikidata.wdtk.datamodel.interfaces._

sealed abstract class EntityDocError(msg: String) extends RuntimeException(msg)
object EntityDocError {
  case class NotTermedDocument(ed: EntityDocument)
      extends EntityDocError(s"Expected TermedDocument but found: $ed")
}
